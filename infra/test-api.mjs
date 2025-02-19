// -*- mode: js -*-

import { strict as assert } from 'node:assert';  // use strict equality everywhere 
import { spawn } from 'child_process';
import net from 'net';

/* Step 1: we start the Herbie server for testing */

function getFreePort() {
    return new Promise((resolve, reject) => {
        const server = net.createServer();
        server.listen(0, () => {
            const { port } = server.address();
            server.close(() => resolve(port));
        });
        server.on('error', reject);
    });
}

const PORT = await getFreePort();

console.log("Spawning server on port " + PORT)
const child = spawn('racket', ['-y', 'src/main.rkt', 'web', '--threads', '2', '--quiet', '--port', ""+PORT]);

child.stdout.on('data', (data) => {
    console.log(""+data);
});

child.stderr.on('data', (data) => {
    console.error(""+data);
});

child.on('close', (code) => {
    if (code) console.log("Server crashed with code " + code);
});

process.on('exit', () => {
  child.kill('SIGTERM'); // or any appropriate signal
});

function waitForPort(port) {
  return new Promise(resolve => {
    const attempt = () => {
      const socket = net.connect(port, 'localhost', () => {
        socket.end();
        resolve();
      });
      socket.on('error', () => setTimeout(attempt, 500));
    };
    attempt();
  });
}

await waitForPort(PORT)
console.log("Server up and responding on port " + PORT)

function makeURL(endpoint) {
  return new URL(`http://127.0.0.1:${PORT}${endpoint}`)
}

/* Step 2: Test the formal API */

// Reusable testing data
const SAMPLE_SIZE = 8000

const FPCoreFormula = '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))'
const FPCoreFormula2 = '(FPCore (x) (- (sqrt (+ x 1))))'
const FPCoreFormula3 = '(FPCore (x) (if (<= (- (sqrt (+ x 1.0)) (sqrt x)) 0.05) (* 0.5 (sqrt (/ 1.0 x))) (fma (fma (- 0.125) x 0.5) x (- 1.0 (sqrt x)))))'
const eval_sample = [[[1], -1.4142135623730951]]

async function testAPI(url, body, cb) {
    console.log("Testing endpoint " + url)

    let sync_start = Date.now()
    const sync_resp = await fetch(makeURL(url), {
        method: 'POST',
        body: JSON.stringify(body),
    });
    assert.ok(sync_resp.headers.get("x-herbie-job-id"));
    const sync_json = await sync_resp.json();
    assert.equal(sync_json.job.length > 0, true)
    assert.equal(sync_json.path.includes("."), true)

    console.log("  Synchronous response in " + Math.round(Date.now() - sync_start) + "ms")
    cb(sync_json);

    let async_start = Date.now()
    let start_resp = await fetch(makeURL(url.replace("/api", "/api/start")), {
        method: 'POST',
        body: JSON.stringify(body),
    })
    let jobid = (await start_resp.json()).job;

    // This loop is a sort of 10 second timeout for the test.
    for (let i = 0; i < 100; i++) {
        let status_resp = await fetch(makeURL("/check-status/" + jobid));
        if (status_resp.status == 201) break;
        await new Promise(r => setTimeout(r, 100)); // Wait 100ms
    }
    let async_resp = await fetch(makeURL("/api/result/" + jobid));
    let async_json = await async_resp.json();

    console.log("  Asynchronous response in " + Math.round(Date.now() - async_start) + "ms")
    cb(async_json);
}

let POINTS = null;

// Sample endpoint
await testAPI("/api/sample", {
    formula: FPCoreFormula2,
    seed: 5,
}, (body) => {
    assert.ok(body.points);
    assert.equal(body.points.length, SAMPLE_SIZE);
    if (!POINTS) {
        POINTS = body.points;
    } else {
        // Test reproducibility between sync and async call
        assert.deepEqual(POINTS, body.points);
    }
});

// Explanations endpoint
await testAPI("/api/explanations", {
  formula: FPCoreFormula,
  sample: POINTS
}, (body) => {
  assert.equal(body.explanation.length > 0, true, 'explanation should not be empty');
});

// Analyze endpoint
await testAPI("/api/analyze", {
  formula: FPCoreFormula,
  sample: [[[14.97651307489794], 0.12711304680349078]]
}, (body) => {
  assert.deepEqual(body.points, [[[14.97651307489794], "2.3"]]);
});

// Exacts endpoint
await testAPI("/api/exacts", {
  formula: FPCoreFormula2,
  sample: eval_sample
}, (body) => {
  assert.deepEqual(body.points, [[[1], -1.4142135623730951]]);
});

// Calculate endpoint
await testAPI("/api/calculate", {
  formula: FPCoreFormula2,
  sample: eval_sample
}, (body) => {
  assert.deepEqual(body.points, [[[1], -1.4142135623730951]]);
});


// Local error endpoint
await testAPI("/api/localerror", {
  formula: FPCoreFormula,
  sample: eval_sample
}, (body) => {
  assert.ok(body.tree['avg-error'] > 0);
});

const localError1 = await (await fetch(makeURL("/api/localerror"), {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[2.852044568544089e-150], 1e+308]], seed: 5
  })
})).json()
const localError2 = await (await fetch(makeURL("/api/localerror"), {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[1.5223342548065899e-15], 1e+308]], seed: 5
  })
})).json()
// Test that different sample points produce different job ids ensuring that different results are served for these inputs.
assert.notEqual(localError1.job, localError2.job)
// Assert local error works for default example.
const ignoredValue = 1e+308
'(FPCore (1e-100) (- (sqrt (+ x 1)) (sqrt x)))'
const localError5 = await (await fetch(makeURL("/api/localerror"), {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[1e-100], ignoredValue]], seed: 5
  })
})).json()

// avg_error, actual_value, exact_value, absolute_difference, ulps_error
// root node
checkLocalErrorNode(localError5.tree, [],
  '-', '0.0', '1.0', '1.0', '1e-50', '0.0')
// left sqrt
checkLocalErrorNode(localError5.tree, [0],
  'sqrt', '0.0', '1.0', '1.0', '5e-101', '0.0')
// right sqrt 
checkLocalErrorNode(localError5.tree, [1],
  'sqrt', '0.0', '1e-50', '1e-50', '2.379726195519099e-68', '0.0')
// plus 
checkLocalErrorNode(localError5.tree, [0, 0],
  '+', '0.0', '1.0', '1.0', '1e-100', '0.0')
// var x
checkLocalErrorNode(localError5.tree, [0, 0, 0],
  'x', '0.0', '1e-100', '1e-100', 'equal', '0.0')
// literal 1
checkLocalErrorNode(localError5.tree, [0, 0, 1],
  '1.0', '0.0', '1.0', '1.0', 'equal', '0.0')

// '(FPCore (1e100) (- (sqrt (+ x 1)) (sqrt x)))'
const localError6 = await (await fetch(makeURL("/api/localerror"), {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[1e100], ignoredValue]], seed: 5
  })
})).json()
// avg_error, actual_value, exact_value, absolute_error, ulps_error
// root node
checkLocalErrorNode(localError6.tree, [],
  '-', '61.7', '0.0', '5e-51', '5e-51', '61.74124908607812')
// left sqrt
checkLocalErrorNode(localError6.tree, [0],
  'sqrt', '0.0', '1e+50', '1e+50', '6.834625285603891e+33', '0.0')
// right sqrt 
checkLocalErrorNode(localError6.tree, [1],
  'sqrt', '0.0', '1e+50', '1e+50', '6.834625285603891e+33', '0.0')
// plus 
checkLocalErrorNode(localError6.tree, [0, 0],
  '+', '0.0', '1e+100', '1e+100', '1.0', '0.0')
// var x
checkLocalErrorNode(localError6.tree, [0, 0, 0],
  'x', '0.0', '1e+100', '1e+100', 'equal', '0.0')
// literal 1
checkLocalErrorNode(localError6.tree, [0, 0, 1],
  '1.0', '0.0', '1.0', '1.0', 'equal', '0.0')

// Test a large number `2e269` to trigger NaNs in local error
const localError7 = await (await fetch(makeURL("/api/localerror"), {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula3, sample: [[[2e269], ignoredValue]], seed: 5
  })
})).json()
// Test against conditionals expressions
checkLocalErrorNode(localError7.tree, [0],
  '<=', '0.0', 'true', 'true', 'equal', '0.0')
checkLocalErrorNode(localError7.tree, [0, 0],
  '-', '61.2', '0.0', '1.1180339887498948e-135', '1.1180339887498948e-135', '61.16647760559045')
checkLocalErrorNode(localError7.tree, [0, 1],
  '0.05', '0.0', '0.05', '0.05', 'equal', '0.0')
checkLocalErrorNode(localError7.tree, [2],
  'fma', '0.0', '-inf.0', '-inf.0', '+inf.0', '0.0')

/// root: The root node of the local error tree.
/// path: the path to get to the node you want to test.
/// name: Name of the node you are testing
/// avg_error: Average Error
/// actual_value: Value of the node
/// exact_value: The correct evaluation of the expression
/// absolute_difference: The ABS of the error at the node |approx - exact|
/// ulps_error: ulps of error at this node.
function checkLocalErrorNode(root, path, name,
  avg_error, actual_value, exact_value, absolute_difference, ulps_error) {
  const node = getNodeFromPath(root, path)
  // console.log(node) // Helpful for seeing which node is failing a test
  assert.equal(node['e'], name)
  assert.equal(node['avg-error'], avg_error)
  assert.equal(node['actual-value'], actual_value)
  assert.equal(node['exact-value'], exact_value)
  assert.equal(node['abs-error-difference'], absolute_difference)
  assert.equal(node['ulps-error'], ulps_error)
}

function getNodeFromPath(node, path) {
  if (path.length > 0) {
    const index = path.shift()
    const child = node['children'][index]
    return getNodeFromPath(child, path)
  } else {
    return node
  }
}

// Alternatives endpoint
await testAPI("/api/alternatives", {
  formula: FPCoreFormula,
  sample: [[[14.97651307489794], 0.12711304680349078]]
}, (body) => {
  assert.ok(Array.isArray(body.alternatives));
});

// Cost endpoint
await testAPI("/api/cost", {
  formula: FPCoreFormula2,
  sample: eval_sample
}, (body) => {
  assert.ok(body.cost > 0);
});

// MathJS endpoint
const mathjs = await (await fetch(makeURL("/api/mathjs"), {
  method: 'POST', body: JSON.stringify({ formula: FPCoreFormula })
})).json()
assert.equal(mathjs.mathjs, "sqrt(x + 1.0) - sqrt(x)")

console.log("Testing translation API");

async function testTranslate(language, result) {
    let start = Date.now()
    const resp = await fetch(makeURL("/api/translate"), {
        method: 'POST',
        body: JSON.stringify({
            formula: FPCoreFormula,
            language: language,
        }),
    });
    const json = await resp.json();

    console.log("  Translation to " + language + " in " + Math.round(Date.now() - start) + "ms")
    assert.equal(json.result.trim().replace("\t", "    "), result.trim());
}

await testTranslate("python", `
def expr(x):
    return math.sqrt((x + 1.0)) - math.sqrt(x)`);

await testTranslate("c", `
double expr(double x) {
    return sqrt((x + 1.0)) - sqrt(x);
}`);

await testTranslate("fortran", `
real(8) function expr(x)
use fmin_fmax_functions
    real(8), intent (in) :: x
    expr = sqrt((x + 1.0d0)) - sqrt(x)
end function`);

await testTranslate("java", `
public static double expr(double x) {
    return Math.sqrt((x + 1.0)) - Math.sqrt(x);
}`);

await testTranslate("julia", `
function expr(x)
    return Float64(sqrt(Float64(x + 1.0)) - sqrt(x))
end`);

await testTranslate("matlab", `
function tmp = expr(x)
    tmp = sqrt((x + 1.0)) - sqrt(x);
end`);

await testTranslate("wls", 'expr[x_] := N[(N[Sqrt[N[(x + 1), $MachinePrecision]], $MachinePrecision] - N[Sqrt[x], $MachinePrecision]), $MachinePrecision]')

await testTranslate("tex", '\\mathsf{expr}\\left(x\\right) = \\sqrt{x + 1} - \\sqrt{x}')

/* Step 3: Test the legacy HTTP endpoints */

console.log("Testing legacy endpoints")

// improve endpoint
const improveResponse = await fetch(makeURL(`/improve?formula=${encodeURIComponent(FPCoreFormula2)}`), { method: 'GET' })
assert.equal(improveResponse.status, 200)
let redirect = improveResponse.url.split("/")
const jobID = redirect[3].split(".")[0]
// This test is a little flaky as the character count of the response is not consistent.
// const improveHTML = await improveResponse.text()
// const improveHTMLexpectedCount = 25871
// assert.equal(improveHTML.length, improveHTMLexpectedCount, `HTML response character count should be ${improveHTMLexpectedCount} unless HTML changes.`)

// timeline
const timelineRSP = await fetch(makeURL(`/timeline/${jobID}`), { method: 'GET' })
assert.equal(timelineRSP.status, 201)
const timeline = await timelineRSP.json()
assert.equal(timeline.length > 0, true)

// Test with a likely missing job-id
const badTimelineRSP = await fetch(makeURL("/timeline/42069"), { method: 'GET' })
assert.equal(badTimelineRSP.status, 404)
const check_missing_job = await fetch(makeURL(`/check-status/42069`), { method: 'GET' })
assert.equal(check_missing_job.status, 202)

// improve-start endpoint
const URIencodedBody = "formula=" + encodeURIComponent(FPCoreFormula)
const startResponse = await fetch(makeURL("/api/start/improve"), {
  method: 'POST',
  headers: {
    'Content-Type': 'application/x-www-form-urlencoded',
  },
  body: URIencodedBody
})
const testResult = (startResponse.status == 201 || startResponse.status == 202)
assert.equal(testResult, true)
const improveResultPath = startResponse.headers.get("location")
let counter = 0
let cap = 100
// Check status endpoint
let checkStatus = await fetch(makeURL(improveResultPath), { method: 'GET' })
/*
This is testing if the /api/start/improve test at the beginning has been completed. The cap and counter is a sort of timeout for the test. Ends up being 10 seconds max.
*/
while (checkStatus.status != 201 && counter < cap) {
  counter += 1
  checkStatus = await fetch(makeURL(improveResultPath), { method: 'GET' })
  await new Promise(r => setTimeout(r, 100)); // ms
}
assert.equal(checkStatus.statusText, 'Job complete')

// up endpoint
const up = await fetch(makeURL("/up"), { method: 'GET' })
assert.equal('Up', up.statusText) // Herbie runs single thread on CI.

// Results.json endpoint
const jsonResults = await (await fetch(makeURL("/results.json"), { method: 'GET' })).json()

// Basic test that checks that there are the two results after the above test.
// TODO add a way to reset the results.json file?
assert.equal(jsonResults.tests.length, 2)


child.kill('SIGINT');
