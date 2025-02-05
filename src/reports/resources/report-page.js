/* The report page uses a dummy-React pattern, where every time the
 * page changes we regenerate the whole HTML content from scratch */

// Here is the loaded data:

var compareAgainstURL = ""
var diffAgainstFields = {}
var otherJsonData = null
var resultsJsonData = null

function update() {
    let bodyNode = document.querySelector("body");
    bodyNode.replaceChildren.apply(bodyNode, buildBody(resultsJsonData, otherJsonData));
}

// Here is the UI state:

const filterNames = {
    "imp-start": "Improved start",
    "apx-start": "Approximate start",
    "uni-start": "Regressed from start",
    "ex-start": "Exact start",
    "eq-start": "Equal start",
    "lt-start": "Less than start",
    "gt-target": "Greater than target",
    "gt-start": "Greater than start",
    "eq-target": "Equal to target",
    "lt-target": "Less than target",
    "error": "Error",
    "timeout": "Timeout",
    "crash": "Crash",
}
var filterState = {
    "imp-start": true,
    "ex-start": true,
    "eq-start": true,
    "eq-target": true,
    "gt-target": true,
    "gt-start": true,
    "uni-start": true,
    "lt-target": true,
    "lt-start": true,
    "apx-start": true,
    "timeout": true,
    "crash": true,
    "error": true,
}

const filterGroups = {
    improved: [
        "ex-start", "eq-start", "eq-target",
        "imp-start", "gt-target", "gt-start",
    ],
    unchanged: [
        "lt-target", "apx-start", "error",
    ],
    regressed: [
        "uni-start", "lt-start", "timeout", "crash",
    ],
}
var filterGroupState = {
    "improved": true,
    "unchanged": true,
    "regressed": true,
}

const radioStates = {
    output: { title: "Output Expression", tolerance: false, },
    startAcc: { title: "Input Accuracy", tolerance: "%", },
    endAcc: { title: "Output Accuracy", tolerance: "%", },
    targetAcc: { title: "Target Accuracy", tolerance: "%", },
    time: { title: "Running Time", tolerance: "s", },
}
let radioState = null;

// Controlling the diff process
var filterTolerance = 1;
var hideDirtyEqual = true

// Collapsable <details> elements
var showFilterDetails = false;
var showCompareDetails = false;


var filterBySuite = ""
var allSuites = []

var filterByWarning = ""
var allWarnings = []

var sortState = {
    key: "test",
    dir: true, // true = ascending, false = descending
}

// Next are various strings used in the UI

const tempXY_A = "Output vs Input Accuracy"
const tempXY_B = "Each point represents a Herbie run below. Its horizontal position shows initial accuracy, and vertical position shows final accuracy. Points above the line are improved by Herbie."

const tempPareto_A = "Accuracy vs Speed"
const tempPareto_B = "A joint speed-accuracy pareto curve. Accuracy is on the vertical axis, speed is on the horizontal axis. Up and to the right is better. The initial program is shown by the red square."

const resultHelpText = `Color key:
    Green: improved accuracy
    Light green: no initial error
    Orange: no accuracy change
    Red: accuracy worsened
    Gray: timeout
    Dark Gray: error`
const targetHelpText = `Color key:
    Dark green: better than target
    Green: matched target
    Orange: improved but did not match target
    Yellow: no accuracy change
    `

// Helper functions of various sorts

function Element(tagname, props, children) {
    if (children === undefined) { children = props; props = {}; }

    var $elt = document.createElement(tagname);
    for (var i in props) if (props.hasOwnProperty(i)) $elt[i] = props[i];

    function addAll(c) {
        if (!c) return;
        else if (Array.isArray(c)) c.map(addAll);
        else if (typeof c == "string") $elt.appendChild(document.createTextNode(c))
        else if (c instanceof Node) $elt.appendChild(c);
        else {
            console.error("Not an element: ", c);
            throw "Invalid element!"
        }
    }
    addAll(children);
    return $elt;
}

function calculatePercent(decimal) {
    return ((100 - (100 * (decimal)))).toFixed(1)
}

function formatAccuracy(decimal) {
    return `${calculatePercent(decimal)}%`
}

function formatTime(ms) {
    if (ms > 60_000) {
        return (ms / 60_000).toFixed(1) + "min"
    } else {
        return (ms / 1000).toFixed(1) + "s"
    }
}

function calculateSpeedup(mergedCostAccuracy) {
    const initial_accuracy = mergedCostAccuracy[0][1]
    const deepCopy = JSON.parse(JSON.stringify(mergedCostAccuracy[1]))
    for (const point of deepCopy.reverse()) {
        if (point[1] > initial_accuracy) {
            return point[0].toFixed(1) + "×"
        }
    }
}

//https://stackoverflow.com/questions/196972/convert-string-to-title-case-with-javascript
function toTitleCase(str) {
    return str.replace(
        /\w\S*/g,
        function (txt) {
            return txt.charAt(0).toUpperCase() + txt.substr(1).toLowerCase();
        }
    )
}

// Based on https://observablehq.com/@fil/plot-onclick-experimental-plugin
// However, simplified because we don't need hit box data
function on(mark, listeners = {}) {
    const render = mark.render
    mark.render = function (facet, { x, y }, channels) {
        const data = this.data
        const g = render.apply(this, arguments)
        const r = d3.select(g).selectChildren()
        for (const [type, callback] of Object.entries(listeners)) {
            r.on(type, function (event, i) {
                return callback(event, data[i])
            })
        }
        return g
    }
    return mark
}

function plotXY(testsData, filterFunction) {
    var filteredTests = []
    testsData.tests.forEach((test) => {
        // Ugh global scope 
        let other = diffAgainstFields[test.name]
        if (filterFunction(test, other)) {
            filteredTests.push(test)
        }
    })
    const out = Plot.plot({
        marks: [
            Plot.line([[0, 0], [1, 1]], { stroke: '#ddd' }),
            on(Plot.dot(filteredTests, {
                x: d => 1 - d.start / 64, y: d => 1 - d.end / 64,
                fill: "#00a", strokeWidth: 2,
            }), {
                click: (e, d) => { window.location = d.link + "/graph.html"; },
            }),
        ],
        className: "clickable",
        marginBottom: 0,
        marginRight: 0,
        width: '400',
        height: '400',
        x: { nice: true, line: true, tickFormat: "%", },
        y: { nice: true, line: true, tickFormat: "%", },
    })
    out.setAttribute('viewBox', '0 0 420 420')
    return out;
}

function plotPareto(jsonData) {
    const [initial, frontier] = jsonData["merged-cost-accuracy"];
    const out = Plot.plot({
        marks: [
            Plot.dot([initial], {
                stroke: "#d00",
                symbol: "square",
                strokeWidth: 2
            }),
            Plot.line(frontier, {
                stroke: "#00a",
                strokeWidth: 2,
            }),
        ],
        width: '400',
        height: '400',
        x: { line: true, nice: true, tickFormat: c => c + "×" },
        y: { line: true, nice: true, domain: [0, 1], tickFormat: "%", },
        marginBottom: 0,
        marginRight: 0,
    })
    out.setAttribute('viewBox', '0 0 420 420')
    return out;
}

function buildCheckboxLabel(classes, text, boolState) {
    return Element("label", { classList: classes }, [
        Element("input", { type: "checkbox", checked: boolState }, []),
        text,
    ]);
}

function buildDiffLine(jsonData, show) {
    const urlInput = Element("input", {
        id: "compare-input", value: compareAgainstURL,
        placeholder: "URL to other json file"
    }, []);

    var unitText = radioStates[radioState]?.tolerance;

    const submitButton = Element("button", "Compute Diff")
    submitButton.addEventListener("click", async (e) => {
        e.preventDefault();
        compareAgainstURL = urlInput.value;
        radioState = radioState ?? "endAcc";
        fetchAndUpdate();
    });

    const toleranceInputField = Element("input", {
        id: `toleranceID`, value: filterTolerance,
        size: 10, style: "text-align:right;",
    }, []);
    toleranceInputField.addEventListener("change", (e) => {
        filterTolerance = toleranceInputField.value;
        update();
    });

    return [
        urlInput,
        unitText && [" Hiding: ±", toleranceInputField, unitText],
        " ", submitButton
    ];
}

function buildCompareForm(jsonData) {
    const formName = "compare-form"

    let radioButtons = [];
    for (let [i, stateInfo] of Object.entries(radioStates)) {
        let radioElt = Element("input", {
            name: formName, id: "compare-" + i,
            type: "radio",
            checked: radioState == i,
        }, []);
        radioElt.addEventListener("click", (e) => {
            radioState = i;
            update();
        });

        let labelElt = Element("label", [radioElt, stateInfo.title]);
        radioButtons.push(labelElt);
    }

    const hideEqual = buildCheckboxLabel("hide-equal", "Hide equal", hideDirtyEqual)
    hideEqual.addEventListener("click", (e) => {
        hideDirtyEqual = !hideDirtyEqual
        update()
    })

    return Element("form", {}, [radioButtons, " ", hideEqual]);
}

function buildBody(jsonData, otherJsonData) {
    let filterFunction = makeFilterFunction();

    function hasNote(note) {
        return (note ? toTitleCase(note) + " " : "") + "Results"
    }

    var total_start = 0
    var total_result = 0
    var maximum_accuracy = 0
    var total_time = 0
    var total_crash_timeout = 0
    jsonData.tests.forEach((test) => {
        total_start += test.start
        total_result += test.end
        maximum_accuracy += test.bits
        total_time += test.time
        if (test.status == "timeout" || test.status == "crash") {
            total_crash_timeout += 1
        }
    })

    const stats = Element("div", { id: "large" }, [
        Element("div", {}, [
            "Average Percentage Accurate: ",
            Element("span", { classList: "number" }, [
                formatAccuracy(total_start / maximum_accuracy),
                Element("span", { classList: "unit" }, [" → ",]),
                formatAccuracy(total_result / maximum_accuracy),]),
        ]),
        Element("div", {}, [
            "Time:",
            Element("span", { classList: "number" }, [formatTime(total_time)])
        ]),
        Element("div", {}, [
            "Bad Runs:",
            Element("span", { classList: "number", title: "Crashes and timeouts are considered bad runs." }, [`${total_crash_timeout}/${jsonData.tests.length}`])
        ]),
        Element("div", {}, [
            "Speedup:",
            Element("span", {
                classList: "number",
                title: "Aggregate speedup of fastest alternative that improves accuracy."
            }, [calculateSpeedup(jsonData["merged-cost-accuracy"])])
        ]),
    ])

    const header = Element("header", {}, [
        Element("h1", {}, hasNote(jsonData.note)),
        Element("img", { src: "logo-car.png" }, []),
        Element("nav", {}, [
            Element("ul", {}, [Element("li", {}, [Element("a", { href: "timeline.html" }, ["Metrics"])])])
        ]),
    ])

    const figureRow = Element("div", { classList: "figure-row" }, [
        Element("figure", { id: "xy" }, [
            Element("h2", {}, [tempXY_A]),
            plotXY(jsonData, filterFunction),
            Element("figcaption", {}, [tempXY_B])
        ]),
        Element("figure", { id: "pareto" }, [
            Element("h2", {}, [tempPareto_A]),
            plotPareto(jsonData),
            Element("figcaption", {}, [tempPareto_B])
        ])
    ])

    function buildTableHeader(stringName, help) {
        const textElement = Element("th", {}, [
            toTitleCase(stringName),
            (stringName != sortState.key ? "–" : sortState.dir ?  "⏶" : "⏷"),
            help && Element("span", { classList: "help-button", title: help }, ["?"]),
        ]);
        textElement.addEventListener("click", (e) => {
            if (stringName == sortState.key) {
                sortState.dir = !sortState.dir;
            } else {
                sortState.key = stringName;
                sortState.dir = true;
            }
            update();
        })
        return textElement
    }

    const rows = buildTableContents(jsonData, otherJsonData, filterFunction)
    const resultsTable = Element("table", { id: "results" }, [
        Element("thead", {}, [
            Element("tr", {}, [
                buildTableHeader("name"),
                buildTableHeader("start"),
                buildTableHeader("end", resultHelpText),
                buildTableHeader("target", targetHelpText),
                buildTableHeader("time"),
            ]),
        ]),
        rows
    ]);
    return [header, stats, figureRow, buildControls(jsonData, rows.length), resultsTable]
}

function compareTests(l, r) {
    let cmp;
    if (sortState.key == "name") {
        cmp = l.name.localeCompare(r.name);
    } else {
        if (l[sortState.key] === false) {
            cmp = 1;
        } else if (r[sortState.key] === false) {
            cmp = -1;
        } else {
            cmp = l[sortState.key] - r[sortState.key];
        }
    }
    if (sortState.dir) cmp = -cmp;
    return cmp;
}

function buildTableContents(jsonData, otherJsonData, filterFunction) {
    var rows = []
    const jsonTest = jsonData.tests.sort(compareTests);
    for (let test of jsonTest) {
        let other = diffAgainstFields[test.name]
        if (filterFunction(test, other)) {
            let row = buildRow(test, other)
            rows.push(row)
        }
    }
    return rows
}

function getMinimum(target) {
    if (target === false) {
        return false
    }

    return target.reduce((minA, current) => {
        const currentA = current[1];
        return currentA < minA ? currentA : minA;
    }, Infinity);
}

// HACK I kinda hate this split lambda function, Zane
function buildRow(test, other) {
    var row
    var smallestTarget = getMinimum(test.target)

    eitherOr(test, other,
        (function () {
            var startAccuracy = formatAccuracy(test.start / test.bits)
            var resultAccuracy = formatAccuracy(test.end / test.bits)

            var targetAccuracy = formatAccuracy(smallestTarget / test.bits)

            if (test.status == "imp-start" || test.status == "ex-start" || test.status == "apx-start") {
                targetAccuracy = ""
            }
            if (test.status == "timeout" || test.status == "error") {
                startAccuracy = ""
                resultAccuracy = ""
                targetAccuracy = ""
            }
            const tr = Element("tr", { classList: test.status }, [
                Element("td", {}, [test.name]),
                Element("td", {}, [startAccuracy]),
                Element("td", {}, [resultAccuracy]),
                Element("td", {}, [targetAccuracy]),
                Element("td", {}, [formatTime(test.time)]),
                Element("td", {}, [
                    Element("a", {
                        href: `${test.link}/graph.html`
                    }, ["»"])]),
                Element("td", {}, [
                    Element("a", {
                        href: `${test.link}/timeline.html`
                    }, ["📊"])]),
            ])
            // TODO fix bug with cmd/ctrl click.
            tr.addEventListener("click", () => tr.querySelector("a").click())
            row = tr
        })
        , (function () {
            function timeTD(test) {
                var timeDiff = test.time - diffAgainstFields[test.name].time
                var color = "diff-time-red"
                var text
                var titleText = `current: ${formatTime(test.time)} vs ${formatTime(diffAgainstFields[test.name].time)}`
                // Dirty equal less then 1 second
                var areEqual = false
                if (Math.abs(timeDiff) < (filterTolerance * 1000)) {
                    areEqual = true
                    color = "diff-time-gray"
                    text = "~"
                } else if (timeDiff < 0) {
                    color = "diff-time-green"
                    text = "+ " + `${formatTime(Math.abs(timeDiff))}`
                } else {
                    text = "-" + `${formatTime(timeDiff)}`
                }
                return { td: Element("td", { classList: color, title: titleText }, [text]), equal: areEqual }
            }

            function buildTDfor(o, t) {
                const op = calculatePercent(o)
                const tp = calculatePercent(t)
                var color = "diff-time-red"
                var diff = op - tp
                var areEqual = false
                var titleText = `Original: ${op} vs ${tp}`
                var tdText = `- ${(diff).toFixed(1)}%`
                if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                    color = "diff-time-gray"
                    areEqual = true
                    tdText = "~"
                } else if (diff < 0) {
                    diff = Math.abs(diff)
                    color = "diff-time-green"
                    tdText = `+ ${(diff).toFixed(1)}%`
                }
                return { td: Element("td", { classList: color, title: titleText }, [tdText]), equal: areEqual }
            }

            function startAccuracyTD(test) {
                const t = test.start / test.bits
                const o = diffAgainstFields[test.name].start / diffAgainstFields[test.name].bits
                return buildTDfor(o, t)
            }

            function resultAccuracyTD(test) {
                const t = test.end / test.bits
                const o = diffAgainstFields[test.name].end / diffAgainstFields[test.name].bits
                return buildTDfor(o, t)
            }

            function targetAccuracyTD(test) {
                const t = smallestTarget / test.bits
                const o = diffAgainstFields[test.name].target / diffAgainstFields[test.name].bits
                return buildTDfor(o, t)
            }

            var classList = [test.status]
            const startAccuracy = startAccuracyTD(test)
            const resultAccuracy = resultAccuracyTD(test)
            const targetAccuracy = targetAccuracyTD(test)
            const time = timeTD(test)

            var tdStartAccuracy = radioState == "startAcc" ? startAccuracy.td : Element("td", {}, [formatAccuracy(test.start / test.bits)])
            var tdResultAccuracy = radioState == "endAcc" ? resultAccuracy.td : Element("td", {}, [formatAccuracy(test.end / test.bits)])
            var tdTargetAccuracy = radioState == "targetAcc" ? targetAccuracy.td : Element("td", {}, [formatAccuracy(smallestTarget / test.bits)])
            const tdTime = radioState == "time" ? time.td : Element("td", {}, [formatTime(test.time)])

            var testTile = ""
            var outputEqual = true
            if (radioState == "output") {
                outputEqual = false
            }
            if (test.output != diffAgainstFields[test.name].output) {
                // TODO steal Latex formatting from Odyssey
                testTile += `Current output:\n${test.output} \n \n Comparing to:\n ${diffAgainstFields[test.name].output}`
            }
            if (test.status == "imp-start" ||
                test.status == "ex-start" ||
                test.status == "apx-start") {
                tdTargetAccuracy = Element("td", {}, [])
            }
            if (test.status == "timeout" || test.status == "error") {
                tdStartAccuracy = Element("td", {}, [])
                tdResultAccuracy = Element("td", {}, [])
                tdTargetAccuracy = Element("td", {}, [])
            }

            const radioSelected = radioState

            var nameTD = Element("td", {}, [test.name])
            if (testTile != "") {
                nameTD = Element("td", { title: testTile }, [test.name])
            }

            const tr = Element("tr", { classList: classList.join(" ") }, [
                nameTD,
                tdStartAccuracy,
                tdResultAccuracy,
                tdTargetAccuracy,
                tdTime,
                Element("td", {}, [
                    Element("a", {
                        href: `${test.link}/graph.html`
                    }, ["»"])]),
                Element("td", {}, [
                    Element("a", {
                        href: `${test.link}/timeline.html`
                    }, ["📊"])]),
            ])
            tr.addEventListener("click", () => tr.querySelector("a").click())
            row = tr
        })
    )
    return row
}

function buildDiffControls(jsonData) {
    var summary = Element("details", { open: showCompareDetails }, [
        Element("summary", {}, [
            Element("h2", {}, ["Diff"]),
            buildDiffLine(jsonData),
        ]),
        buildCompareForm(jsonData),
    ])

    summary.addEventListener("toggle", (e) => {
        showCompareDetails = summary.open;
    });

    return summary;
}

function buildControls(jsonData, diffCount) {
    const showing = diffCount + "/" + jsonData.tests.length;
    var displayingDiv = Element("div", [
        "Displaying " + showing + " benchmarks",
        " on ", Element("code", jsonData.branch),
        otherJsonData && [
            ", compared with baseline ", Element("code", otherJsonData.branch),
        ],
    ])

    return Element("div", { classList: "report-details" }, [
        displayingDiv,
        buildDiffControls(jsonData),
        buildFilterControls(jsonData),
    ])
}

function buildFilterGroup(jsonData, name, childStateNames) {
    let subFilters = filterGroups[name];
    let label = buildCheckboxLabel(name, toTitleCase(name), filterGroupState[name]);
    label.addEventListener("click", (e) => {
        filterGroupState[name] = e.target.checked;
        for (let filterName of subFilters) {
            filterState[filterName] = e.target.checked;
        }
        update();
    });
    return label;
}

function buildFilterControls(jsonData) {
    var testTypeCounts = {}
    for (let test of jsonData.tests) {
        testTypeCounts[test.status] == null ?
            testTypeCounts[test.status] = 1 :
            testTypeCounts[test.status] += 1
    }

    var filterButtons = []
    for (let f in filterState) {
        const name = `${filterNames[f]} (${testTypeCounts[f] ? testTypeCounts[f] : "0"})`
        const button = buildCheckboxLabel(f + " sub-filter", name, filterState[f])
        button.addEventListener("click", () => {
            filterState[f] = button.querySelector("input").checked
            update()
        })
        filterButtons.push(button)
    }

    const dropDown = Element("select", [
        Element("option", { value: "", selected: !filterBySuite }, ["Filter by suite"]),
        allSuites.map((suite, i) => 
            Element("option", { value: suite, selected: filterBySuite == suite }, [toTitleCase(suite)]))
    ]);
    dropDown.addEventListener("input", (e) => {
        filterBySuite = dropDown.value ?? "";
        update();
    });

    const dropDown2 = Element("select", [
        Element("option", { value: "", selected: !filterByWarning }, ["Filter to warning"]),
        allWarnings.map((name) => 
            Element("option", { value: name, selected: filterByWarning == name }, [name]))
    ]);
    dropDown2.addEventListener("input", (e) => {
        filterByWarning = dropDown2.value ?? "";
        update();
    });

    let groupButtons = [];
    for (let i in filterGroupState) {
        groupButtons.push(buildFilterGroup(jsonData, i));
    }

    const filters = Element("details", { id: "filters", open: showFilterDetails }, [
        Element("summary", {}, [
            Element("h2", {}, "Filters"),
            groupButtons, " ", dropDown, " ", dropDown2,
        ]),
        filterButtons,
    ]);
    filters.addEventListener("toggle", (e) => {
        showFilterDetails = filters.open;
    });
    return filters;
}

function eitherOr(baselineRow, diffRow, singleFunction, pairFunctions) {
    // Pulled out into a function so if testing for diffRow needs to change only have to update here
    if (diffRow == undefined) {
        singleFunction()
    } else {
        pairFunctions()
    }
}

function showGetJsonError(error) {
    const header = Element("header", {}, [
        Element("h1", {}, "Error loading results"),
        Element("img", { src: "logo-car.png" }, []),
        Element("nav", {}, [
            Element("ul", {}, [Element("li", {}, [Element("a", { href: "timeline.html" }, ["Metrics"])])])
        ]),
    ])

    let is_windows = navigator.userAgent.indexOf("Windows") !== -1;
    let page_name = window.location.pathname.split("/").at(-1);
    let page_location;
    if (is_windows) {
        page_location = window.location.pathname.split("/").slice(1, -1).join("\\");
    } else {
        page_location = window.location.pathname.split("/").slice(0, -1).join("/");
    }

    let reason;
    if (window.location.protocol == "file:") {
        reason = [
            Element("p", [
                "Modern browsers prevent access over ",
                Element("code", "file://"), " URLs, which Herbie requires.",
            ]),
            Element("p", [
                "You can work around this by starting a local server, like so:"
            ]),
            Element("pre", [
                is_windows // Python on windows is usually called python, not python3
                    ? "python -m http.server -d " + page_location + " 8123"
                    : "python3 -m http.server -d " + page_location + " 8123"
            ]),
            Element("p", [
                "and then navigating to ",
                Element("a", {
                    href: "http://localhost:8123/" + page_name,
                }, Element("kbd", "http://localhost:8123/" + page_name)),
            ]),
        ];
    }
    
    const message = Element("section", {className: "error"}, [
        Element("h2", "Could not load results"),
        reason,
    ]);

    let body = [header, message];

    let bodyNode = document.querySelector("body");
    if (bodyNode) {
        bodyNode.replaceChildren.apply(bodyNode, body);
    } else {
        document.addEventListener("DOMContentLoaded", () => showGetJsonError(error));
    }
}

function makeFilterFunction() {
    return function filterFunction(baseData, diffData) {
        var returnValue = true

        // Section to hide diffs that are below the provided tolerance
        if (hideDirtyEqual) {
            // Diff Start Accuracy
            if (radioState == "output") {
                if (baseData.output != diffData.output) {
                    returnValue = returnValue && false;
                }
                const t = baseData.start / baseData.bits
                const o = diffData.start / diffData.bits
                const op = calculatePercent(o)
                const tp = calculatePercent(t)
                var diff = op - tp
                if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                    returnValue = returnValue && false;
                }
            }
            // Diff Start Accuracy
            if (radioState == "startAcc") {
                const t = baseData.start / baseData.bits
                const o = diffData.start / diffData.bits
                const op = calculatePercent(o)
                const tp = calculatePercent(t)
                var diff = op - tp
                if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                    returnValue = returnValue && false
                }
            }
            
            // Diff Result Accuracy
            if (radioState == "endAcc") {
                const t = baseData.end / baseData.bits
                const o = diffData.end / diffData.bits
                const op = calculatePercent(o)
                const tp = calculatePercent(t)
                var diff = op - tp
                if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                    returnValue = returnValue && false
                }
            }

            // Diff Target Accuracy
            if (radioState == "targetAcc") {
                var smallestBase = getMinimum(baseData.target)
                var smallestDiff = getMinimum(diffData.target)
                
                const t = smallestBase / baseData.bits
                const o = smallestDiff / diffData.bits
                const op = calculatePercent(o)
                const tp = calculatePercent(t)
                var diff = op - tp
                if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                    returnValue = returnValue && false
                }
            }

            // Diff Time
            if (radioState == "time") {
                var timeDiff = baseData.time - diffData.time
                if (Math.abs(timeDiff) < (filterTolerance * 1000)) {
                    returnValue = returnValue && false
                }
            }
        }

        const linkComponents = baseData.link.split("/")
        // guard statement
        if (filterBySuite && linkComponents.length > 1) {
            // defensive lowerCase
            const left = filterBySuite;
            const right = linkComponents[0]
            if (left.toLowerCase() != right.toLowerCase()) {
                return false
            }
        }

        if (filterByWarning && baseData.warnings.indexOf(filterByWarning) === -1) {
            return false
        }

        if (!filterState[baseData.status]) {
            return false
        }

        return returnValue
    }
}

async function fetchAndUpdate() {
    if (compareAgainstURL) {
        // Could also split string on / and check if the last component = "results.json"
        var url = compareAgainstURL
        if (url.endsWith("/")) url += "results.json"

        let response = await fetch(url, {
            headers: { "content-type": "text/plain" },
            method: "GET",
            mode: "cors",
        })
        const json = await response.json()
        if (json.error) {
            otherJsonData = null
            update()
            return
        }
        for (let test of json.tests) {
            diffAgainstFields[`${test.name}`] = test
        }
        otherJsonData = json
        update()
    } else {
        otherJsonData = null
        update()
    }
}

async function getResultsJson() {
    if (resultsJsonData == null) {
        let response;
        try {
            response = await fetch("results.json", {
                headers: { "content-type": "application/json" },
            });
        } catch (err) {
            return showGetJsonError(err);
        }
        resultsJsonData = (await response.json());
        storeBenchmarks(resultsJsonData.tests)
    }
}

function storeBenchmarks(tests) {
    var tempDir = {}
    var tempAllWarnings = {}
    for (let test of tests) {
        const linkComponents = test.link.split("/")
        if (linkComponents.length > 1) {
            tempDir[linkComponents[0]] = linkComponents[0]
        }
        for (let warning of test.warnings)  {
            tempAllWarnings[warning] = warning
        }
    }
    allSuites = Object.keys(tempDir);
    allWarnings = Object.keys(tempAllWarnings);
    update();
}

getResultsJson()
