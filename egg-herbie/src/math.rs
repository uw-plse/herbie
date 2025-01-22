use egg::*;
use std::sync::atomic::{AtomicBool, Ordering};

use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::Ratio;
use num_traits::{One, Pow, Signed, Zero};
use std::str::FromStr;

pub type Constant = num_rational::BigRational;
pub type RecExpr = egg::RecExpr<Math>;
pub type Pattern = egg::Pattern<Math>;
pub type EGraph = egg::EGraph<Math, ConstantFold>;
pub type Rewrite = egg::Rewrite<Math, ConstantFold>;
pub type Runner = egg::Runner<Math, ConstantFold, IterData>;
pub type Iteration = egg::Iteration<IterData>;

#[derive(Clone)]
pub struct IterData {
    pub extracted: (),
}

#[derive(Clone)]
pub struct Extracted {
    pub best: RecExpr,
    pub cost: usize,
}

// cost function similar to AstSize except it will
// penalize `(pow _ p)` where p is a fraction
pub struct AltCost<'a> {
    pub egraph: &'a EGraph,
}

impl<'a> AltCost<'a> {
    pub fn new(egraph: &'a EGraph) -> Self {
        Self { egraph }
    }
}

impl<'a> CostFunction<Math> for AltCost<'a> {
    type Cost = usize;

    fn cost<C>(&mut self, enode: &Math, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        if let Math::Pow([_, i]) = enode {
            if let Some((n, _reason)) = &self.egraph[*i].data {
                if !n.denom().is_one() && n.denom().is_odd() {
                    return usize::MAX;
                }
            }
        }

        enode.fold(1, |sum, id| usize::saturating_add(sum, costs(id)))
    }
}

impl IterationData<Math, ConstantFold> for IterData {
    fn make(_runner: &Runner) -> Self {
        Self { extracted: () }
    }
}

// operators from FPCore
define_language! {
    pub enum Math {

        // constant-folding operators

        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "pow" = Pow([Id; 2]),
        "neg" = Neg([Id; 1]),
        "sqrt" = Sqrt([Id; 1]),
        "fabs" = Fabs([Id; 1]),
        "ceil" = Ceil([Id; 1]),
        "floor" = Floor([Id; 1]),
        "round" = Round([Id; 1]),
        "log" = Log([Id; 1]),
        "cbrt" = Cbrt([Id; 1]),

        Constant(Constant),
        Symbol(egg::Symbol),
        Other(egg::Symbol, Vec<Id>),
    }
}

pub struct ConstantFold {
    pub unsound: AtomicBool,
    pub constant_fold: bool,
    pub prune: bool,
}

impl Clone for ConstantFold {
    fn clone(&self) -> Self {
        let unsound = AtomicBool::new(self.unsound.load(Ordering::SeqCst));
        Self {
            unsound,
            constant_fold: self.constant_fold,
            prune: self.prune,
        }
    }
}

impl Default for ConstantFold {
    fn default() -> Self {
        Self {
            constant_fold: true,
            prune: true,
            unsound: AtomicBool::new(false),
        }
    }
}

impl Analysis<Math> for ConstantFold {
    type Data = Option<(Constant, (PatternAst<Math>, Subst))>;
    fn make(egraph: &mut EGraph, enode: &Math) -> Self::Data {
        if !egraph.analysis.constant_fold {
            return None;
        }

        let x = |id: &Id| egraph[*id].data.clone().map(|x| x.0);
        let is_zero = |id: &Id| {
            let data = egraph[*id].data.as_ref();
            match data {
                Some(data) => data.0.is_zero(),
                None => false,
            }
        };

        Some((
            match enode {
                Math::Constant(c) => c.clone(),

                // real
                Math::Add([a, b]) => x(a)? + x(b)?,
                Math::Sub([a, b]) => x(a)? - x(b)?,
                Math::Mul([a, b]) => x(a)? * x(b)?,
                Math::Div([a, b]) => {
                    if x(b)?.is_zero() {
                        return None;
                    } else {
                        x(a)? / x(b)?
                    }
                }
                Math::Neg([a]) => -x(a)?,
                Math::Pow([a, b]) => {
                    if is_zero(a) {
                        if x(b)?.is_positive() {
                            Ratio::new(BigInt::from(0), BigInt::from(1))
                        } else {
                            return None;
                        }
                    } else if is_zero(b) {
                        Ratio::new(BigInt::from(1), BigInt::from(1))
                    } else if x(b)?.is_integer() {
                        Pow::pow(x(a)?, x(b)?.to_integer())
                    } else {
                        return None;
                    }
                }
                Math::Sqrt([a]) => {
                    let a = x(a)?;
                    if *a.numer() > BigInt::from(0) && *a.denom() > BigInt::from(0) {
                        let s1 = a.numer().sqrt();
                        let s2 = a.denom().sqrt();
                        let is_perfect = &(&s1 * &s1) == a.numer() && &(&s2 * &s2) == a.denom();
                        if is_perfect {
                            Ratio::new(s1, s2)
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                Math::Log([a]) => {
                    if x(a)? == Ratio::new(BigInt::from(1), BigInt::from(1)) {
                        Ratio::new(BigInt::from(0), BigInt::from(1))
                    } else {
                        return None;
                    }
                }
                Math::Cbrt([a]) => {
                    if x(a)? == Ratio::new(BigInt::from(1), BigInt::from(1)) {
                        Ratio::new(BigInt::from(1), BigInt::from(1))
                    } else {
                        return None;
                    }
                }
                Math::Fabs([a]) => x(a)?.abs(),
                Math::Floor([a]) => x(a)?.floor(),
                Math::Ceil([a]) => x(a)?.ceil(),
                Math::Round([a]) => x(a)?.round(),

                _ => return None,
            },
            {
                let mut pattern: PatternAst<Math> = Default::default();
                let mut var_counter = 0;
                let mut subst: Subst = Default::default();
                enode.for_each(|child| {
                    if let Some(constant) = x(&child) {
                        pattern.add(ENodeOrVar::ENode(Math::Constant(constant)));
                    } else {
                        let var = ("?".to_string() + &var_counter.to_string())
                            .parse()
                            .unwrap();
                        pattern.add(ENodeOrVar::Var(var));
                        subst.insert(var, child);
                        var_counter += 1;
                    }
                });
                let mut counter = 0;
                let mut head = enode.clone();
                head.update_children(|_child| {
                    let res = Id::from(counter);
                    counter += 1;
                    res
                });
                pattern.add(ENodeOrVar::ENode(head));
                (pattern, subst)
            },
        ))
    }

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
        match (&to, from) {
            (None, None) => DidMerge(false, false),
            (Some(_), None) => DidMerge(false, true), // no update needed
            (None, Some(c)) => {
                *to = Some(c);
                DidMerge(true, false)
            }
            (Some(a), Some(ref b)) => {
                if a.0 != b.0 && !self.unsound.swap(true, Ordering::SeqCst) {
                    log::warn!("Bad merge detected: {} != {}", a.0, b.0);
                }
                DidMerge(false, false)
            }
        }
    }

    fn modify(egraph: &mut EGraph, class_id: Id) {
        let class = &mut egraph[class_id];
        if let Some((c, (pat, subst))) = class.data.clone() {
            egraph.union_instantiations(
                &pat,
                &format!("{}", c).parse().unwrap(),
                &subst,
                "metadata-eval".to_string(),
            );

            if egraph.analysis.prune {
                egraph[class_id].nodes.retain(|n| n.node.is_leaf())
            }
        }
    }
}

pub fn mk_rules(tuples: &[(&str, &str, &str)]) -> Vec<Rewrite> {
    tuples
        .iter()
        .map(|(name, left, right)| {
            let left = Pattern::from_str(left).unwrap();
            let right = Pattern::from_str(right).unwrap();
            Rewrite::new(*name, left, right).unwrap()
        })
        .collect()
}
