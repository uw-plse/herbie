#![allow(clippy::missing_safety_doc)]

pub mod math;

use egg::{BackoffScheduler, Extractor, FromOp, Id, Language, SimpleScheduler, StopReason, Symbol};
use indexmap::IndexMap;
use libc::{c_void, strlen};
use math::*;

use std::cmp::min;
use std::ffi::{CStr, CString};
use std::mem::{self, ManuallyDrop};
use std::os::raw::c_char;
use std::time::Duration;
use std::{slice, sync::atomic::Ordering};

pub struct Context {
    iteration: usize,
    runner: Runner,
    rules: Vec<Rewrite>,
}

// I had to add $(rustc --print sysroot)/lib to LD_LIBRARY_PATH to get linking to work after installing rust with rustup
#[no_mangle]
pub unsafe extern "C" fn egraph_create() -> *mut Context {
    Box::into_raw(Box::new(Context {
        iteration: 0,
        runner: Runner::new(Default::default()).with_explanations_enabled(),
        rules: vec![],
    }))
}

#[no_mangle]
pub unsafe extern "C" fn egraph_destroy(ptr: *mut Context) {
    drop(Box::from_raw(ptr))
}

#[no_mangle]
pub unsafe extern "C" fn destroy_egraphiters(ptr: *mut c_void) {
    // TODO: Switch ffi to use `usize` directly to avoid the risk of these being incorrect
    drop(Box::from_raw(ptr as *mut Vec<EGraphIter>));
    // drop(Vec::from_raw_parts(data, length as usize, capacity as usize))
}

#[no_mangle]
pub unsafe extern "C" fn destroy_string(ptr: *mut c_char) {
    drop(CString::from_raw(ptr))
}

#[no_mangle]
pub unsafe extern "C" fn string_length(ptr: *const c_char) -> u32 {
    strlen(ptr) as u32
}

#[repr(C)]
pub struct EGraphIter {
    numnodes: u32,
    numclasses: u32,
    time: f64,
}

// a struct for loading rules from external source
#[repr(C)]
pub struct FFIRule {
    name: *const c_char,
    left: *const c_char,
    right: *const c_char,
}

#[no_mangle]
pub unsafe extern "C" fn egraph_add_expr(ptr: *mut Context, expr: *const c_char) -> u32 {
    let _ = env_logger::try_init();
    // Safety: `ptr` was box allocated by `egraph_create`
    let mut context = Box::from_raw(ptr);

    assert_eq!(context.iteration, 0);
    let rec_expr = CStr::from_ptr(expr).to_str().unwrap().parse().unwrap();
    context.runner = context.runner.with_expr(&rec_expr);
    let id = usize::from(*context.runner.roots.last().unwrap())
        .try_into()
        .unwrap();

    mem::forget(context);

    id
}

#[no_mangle]
pub unsafe extern "C" fn egraph_add_node(
    ptr: *mut Context,
    f: *const c_char,
    ids_ptr: *const u32,
    num_ids: u32,
    is_root: bool,
) -> u32 {
    let _ = env_logger::try_init();
    // Safety: `ptr` was box allocated by `egraph_create`
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));

    let f = CStr::from_ptr(f).to_str().unwrap();
    let len = num_ids as usize;
    let ids: &[u32] = slice::from_raw_parts(ids_ptr, len);
    let ids = ids.iter().map(|id| Id::from(*id as usize)).collect();
    let node = Math::from_op(f, ids).unwrap();
    let id = context.runner.egraph.add(node);
    if is_root {
        context.runner.roots.push(id);
    }

    usize::from(id) as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_copy(ptr: *mut Context) -> *mut Context {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = Box::from_raw(ptr);
    let mut runner = Runner::new(Default::default())
        .with_explanations_enabled()
        .with_egraph(context.runner.egraph.clone());
    runner.roots = context.runner.roots.clone();
    runner.egraph.rebuild();

    mem::forget(context);

    Box::into_raw(Box::new(Context {
        iteration: 0,
        rules: vec![],
        runner,
    }))
}

unsafe fn ptr_to_string(ptr: *const c_char) -> String {
    let bytes = CStr::from_ptr(ptr).to_bytes();
    String::from_utf8(bytes.to_vec()).unwrap()
}

// todo don't just unwrap, also make sure the rules are validly parsed
unsafe fn ffirule_to_tuple(rule_ptr: *mut FFIRule) -> (String, String, String) {
    let rule = &mut *rule_ptr;
    (
        ptr_to_string(rule.name),
        ptr_to_string(rule.left),
        ptr_to_string(rule.right),
    )
}

#[no_mangle]
pub unsafe extern "C" fn egraph_run(
    ptr: *mut Context,
    rules_array_ptr: *const *mut FFIRule,
    rules_array_length: u32,
    iterations_length: *mut u32,
    iterations_ptr: *mut *mut c_void,
    iter_limit: u32,
    node_limit: u32,
    simple_scheduler: bool,
    is_constant_folding_enabled: bool,
) -> *const EGraphIter {
    // Safety: `ptr` was box allocated by `egraph_create`
    let mut context = Box::from_raw(ptr);

    if context.runner.stop_reason.is_none() {
        let length: usize = rules_array_length as usize;
        let ffi_rules: &[*mut FFIRule] = slice::from_raw_parts(rules_array_ptr, length);
        let mut ffi_tuples: Vec<(&str, &str, &str)> = vec![];
        let mut ffi_strings: Vec<(String, String, String)> = vec![];
        for ffi_rule in ffi_rules.iter() {
            let str_tuple = ffirule_to_tuple(*ffi_rule);
            ffi_strings.push(str_tuple);
        }

        for ffi_string in ffi_strings.iter() {
            ffi_tuples.push((&ffi_string.0, &ffi_string.1, &ffi_string.2));
        }

        let rules: Vec<Rewrite> = math::mk_rules(&ffi_tuples);
        context.rules = rules;

        context.runner.egraph.analysis.constant_fold = is_constant_folding_enabled;
        context.runner = if simple_scheduler {
            context.runner.with_scheduler(SimpleScheduler)
        } else {
            context.runner.with_scheduler(BackoffScheduler::default())
        };

        context.runner = context
            .runner
            .with_node_limit(node_limit as usize)
            .with_iter_limit(iter_limit as usize) // should never hit
            .with_time_limit(Duration::from_secs(u64::MAX))
            .with_hook(|r| {
                if r.egraph.analysis.unsound.load(Ordering::SeqCst) {
                    Err("Unsoundness detected".into())
                } else {
                    Ok(())
                }
            })
            .run(&context.rules);
    }

    let iterations = context
        .runner
        .iterations
        .iter()
        .map(|iteration| EGraphIter {
            numnodes: iteration.egraph_nodes as u32,
            numclasses: iteration.egraph_classes as u32,
            time: iteration.total_time,
        })
        .collect::<Vec<_>>();
    let iterations_data = iterations.as_ptr();

    std::ptr::write(iterations_length, iterations.len() as u32);
    std::ptr::write(
        iterations_ptr,
        Box::into_raw(Box::new(iterations)) as *mut c_void,
    );
    mem::forget(context);

    iterations_data
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_stop_reason(ptr: *mut Context) -> u32 {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));

    match context.runner.stop_reason {
        Some(StopReason::Saturated) => 0,
        Some(StopReason::IterationLimit(_)) => 1,
        Some(StopReason::NodeLimit(_)) => 2,
        Some(StopReason::Other(_)) => 3,
        _ => 4,
    }
}

fn find_extracted(runner: &Runner, id: u32, iter: u32) -> &Extracted {
    let id = runner.egraph.find(Id::from(id as usize));

    // go back one more iter, egg can duplicate the final iter in the case of an error
    let is_unsound = runner.egraph.analysis.unsound.load(Ordering::SeqCst);
    let sound_iter = min(
        runner
            .iterations
            .len()
            .saturating_sub(if is_unsound { 3 } else { 1 }),
        iter as usize,
    );

    runner.iterations[sound_iter]
        .data
        .extracted
        .iter()
        .find(|(i, _)| runner.egraph.find(*i) == id)
        .map(|(_, ext)| ext)
        .expect("Couldn't find matching extraction!")
}

#[no_mangle]
pub unsafe extern "C" fn egraph_find(ptr: *mut Context, id: usize) -> u32 {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let node_id = Id::from(id);
    let canon_id = context.runner.egraph.find(node_id);
    usize::from(canon_id) as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_serialize(ptr: *mut Context) -> *const c_char {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let mut ids: Vec<Id> = context.runner.egraph.classes().map(|c| c.id).collect();
    ids.sort();

    // Iterate through the eclasses and print each eclass
    let mut s = String::from("(");
    for id in ids {
        let c = &context.runner.egraph[id];
        s.push_str(&format!("({}", id));
        for node in &c.nodes {
            if matches!(node, Math::Symbol(_) | Math::Constant(_)) {
                s.push_str(&format!(" {}", node));
            } else {
                s.push_str(&format!("({}", node));
                for c in node.children() {
                    s.push_str(&format!(" {}", c));
                }
                s.push(')');
            }
        }

        s.push(')');
    }
    s.push(')');

    let c_string = ManuallyDrop::new(CString::new(s).unwrap());
    c_string.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_size(ptr: *mut Context) -> u32 {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    context.runner.egraph.number_of_classes() as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_eclass_size(ptr: *mut Context, id: u32) -> u32 {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let id = Id::from(id as usize);
    context.runner.egraph[id].nodes.len() as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_enode_size(ptr: *mut Context, id: u32, idx: u32) -> u32 {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let id = Id::from(id as usize);
    let idx = idx as usize;
    context.runner.egraph[id].nodes[idx].len() as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_eclasses(ptr: *mut Context, ids_ptr: *mut u32) {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let mut ids: Vec<u32> = context
        .runner
        .egraph
        .classes()
        .map(|c| usize::from(c.id) as u32)
        .collect();
    ids.sort();

    for (i, id) in ids.iter().enumerate() {
        std::ptr::write(ids_ptr.offset(i as isize), *id);
    }
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_node(
    ptr: *mut Context,
    id: u32,
    idx: u32,
    ids: *mut u32,
) -> *const c_char {
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let id = Id::from(id as usize);
    let idx = idx as usize;

    let node = &context.runner.egraph[id].nodes[idx];
    for (i, id) in node.children().iter().enumerate() {
        std::ptr::write(ids.offset(i as isize), usize::from(*id) as u32);
    }

    let c_string = ManuallyDrop::new(CString::new(node.to_string()).unwrap());
    c_string.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_simplest(
    ptr: *mut Context,
    node_id: u32,
    iter: u32,
) -> *const c_char {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let ext = find_extracted(&context.runner, node_id, iter);
    let best_str = ManuallyDrop::new(CString::new(ext.best.to_string()).unwrap());

    best_str.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_proof(
    ptr: *mut Context,
    expr: *const c_char,
    goal: *const c_char,
) -> *const c_char {
    // Safety: `ptr` was box allocated by `egraph_create`
    let mut context = ManuallyDrop::new(Box::from_raw(ptr));
    // Send `EGraph` since neither `Context` nor `Runner` are `Send`. `Runner::explain_equivalence` just forwards to `EGraph::explain_equivalence` so this is fine.
    let egraph = &mut context.runner.egraph;
    let expr_rec = CStr::from_ptr(expr).to_str().unwrap().parse().unwrap();
    let goal_rec = CStr::from_ptr(goal).to_str().unwrap().parse().unwrap();

    // extract the proof as a tree
    let string = egraph
        .explain_equivalence(&expr_rec, &goal_rec)
        .get_string_with_let()
        .replace('\n', " ");

    let c_string = ManuallyDrop::new(CString::new(string).unwrap());
    c_string.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_variants(
    ptr: *mut Context,
    node_id: u32,
    orig_expr: *const c_char,
) -> *const c_char {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));

    // root (id, expr)
    let id = Id::from(node_id as usize);
    let orig_recexpr: RecExpr = CStr::from_ptr(orig_expr).to_str().unwrap().parse().unwrap();
    let head_node = &orig_recexpr.as_ref()[orig_recexpr.as_ref().len() - 1];

    // extractor
    let extractor = Extractor::new(&context.runner.egraph, AltCost::new(&context.runner.egraph));
    let mut cache: IndexMap<Id, RecExpr> = Default::default();

    // extract variants
    let mut exprs = vec![];
    for n in &context.runner.egraph[id].nodes {
        // assuming same ops in an eclass cannot
        // have different precisions
        if !n.matches(head_node) {
            // extract if not in cache
            n.for_each(|id| {
                if cache.get(&id).is_none() {
                    let (_, best) = extractor.find_best(id);
                    cache.insert(id, best);
                }
            });

            exprs.push(n.join_recexprs(|id| cache.get(&id).unwrap().as_ref()));
        }
    }

    // format
    let expr_strs: Vec<String> = exprs.iter().map(|r| r.to_string()).collect();
    let best_str = ManuallyDrop::new(CString::new(expr_strs.join(" ")).unwrap());

    best_str.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_is_unsound_detected(ptr: *mut Context) -> bool {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));

    context
        .runner
        .egraph
        .analysis
        .unsound
        .load(Ordering::SeqCst)
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_times_applied(ptr: *mut Context, name: *const c_char) -> u32 {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let sym = Symbol::from(ptr_to_string(name));

    context
        .runner
        .iterations
        .iter()
        .map(|iter| *iter.applied.get(&sym).unwrap_or(&0) as u32)
        .sum()
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_cost(ptr: *mut Context, node_id: u32, iter: u32) -> u32 {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));
    let ext = find_extracted(&context.runner, node_id, iter);

    ext.cost as u32
}

#[no_mangle]
pub unsafe extern "C" fn egraph_get_size(ptr: *mut Context) -> u32 {
    // Safety: `ptr` was box allocated by `egraph_create`
    let context = ManuallyDrop::new(Box::from_raw(ptr));

    context
        .runner
        .iterations
        .last()
        .map(|iteration| iteration.egraph_nodes as u32)
        .unwrap_or_default()
}
