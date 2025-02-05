
vmargin = 10;
hmargin = 5;
width = 340;
height = 200;
labels = 50;
precision = 64;
precision_step = 8;

key = undefined;
used_branch = {};
used_tag = {};

function get_point(tr) {
    var tests = tr.children[4].textContent.split("/");
    var bits = tr.children[5].textContent.split("/");
    var flags = tr.children[3].getAttribute("title");
    flags = flags !== "" ? flags.split(" ") : [];

    var note = tr.getElementsByClassName("note")[0];
    var trtag = note && note.textContent;
    
    return {
        tag: trtag,
        tests: { got: +tests[0], total: +tests[1]},
        bits: { got: +bits[0], total: +bits[1] },
        branch: tr.children[2].textContent,
        time: +tr.children[0].children[0].getAttribute("data-unix"),
        speed: +tr.children[1].children[0].getAttribute("data-ms") / 1000 / 60,
        elt: tr,
        flags: flags,
    };
}

function get_data(table) {
    var data = Array.prototype.slice.call(table.querySelectorAll("tbody tr")).map(get_point);
    data.forEach(function(pt) {
        if (pt.tag && pt.branch) used_branch[pt.branch] = (used_branch[pt.branch] || 0) + 1;
        if (pt.tag) used_tag[pt.tag] = (used_tag[pt.tag] || 0) + 1;
    });
    data.sort(function(a,b) {return a.time - b.time});
    return data;
}

function print_date(d) {
    var date = "" + new Date(d * 1000);
    return date.split(" ").slice(1, 4).join(" ");
}

function step_size(max) {
    var step = Math.pow(10, Math.floor(Math.log10(max / 4)))
    if (max / step > 20) return step * 5;
    if (max / step > 8) return step * 2;
    return step;
}

function make_accuracy_graph(node, data, type) {
    if (!data.length) return no_data(node);

    var spacing = width / data.length;
    
    var max = Math.max.apply(null, data.map(function(x) { return x[type].total }))
    var step = step_size(max);
    var steps = max ? Math.ceil(max / step) : 0;
    var max = steps * step;

    var svg = initialize_svg(node);
    add_axes(svg);
    add_gridlines(svg, step, steps, "b");

    var g = mk_datum(svg, data);

    g.append("line")
        .attr("stroke", function(d) { return key(d.branch) })
        .attr("x1", function(d, i) { return (i + .5) * spacing })
        .attr("x2", function(d, i) { return (i + .5) * spacing })
        .attr("y1", function(d) { return height - height * d[type].total / max })
        .attr("y2", function(d) { return height - height * (d[type].total - d[type].got) / max - 5 });

    g.append("polygon").attr("points", "-3.5,-6,3.5,-6,0,0")
        .attr("fill", function(d) { return key(d.branch) })
        .attr("transform", function(d, i) {
            return "translate(" + spacing*(i + .5) + ", " + (height - height * (d[type].total - d[type].got) / max) + ")";
        });
}

function no_data(node) {
    node.append("text")
        .attr("x", width / 2)
        .attr("y", height / 2 + 9)
        .attr("class", "no-data").text("No tests found with these parameters.");
}

function initialize_svg(node) {
    return node
        .attr("width", width + 2 * hmargin + labels)
        .attr("height", height + 2 * vmargin)
        .append("g").attr("transform", "translate(" + (hmargin + labels) + "," + vmargin + ")");
}

function add_axes(svg) {
    svg.append("line")
        .attr("class", "gridline")
        .attr("x1", 0)
        .attr("x2", width-5)
        .attr("y1", height)
        .attr("y2", height);

    svg.append("polygon").attr("class", "gridline").attr("points", "0,3,0,-3,5,0")
        .attr("transform", "translate(" + (width - 5) + "," + height + ")");
}

function add_gridlines(svg, step, steps, unit) {
    for (var i = 1; i <= steps; i++) {
        svg.append("line")
            .attr("class", "guide")
            .attr("x1", 0)
            .attr("x2", width)
            .attr("y1", height - (i / steps) * height)
            .attr("y2", height - (i / steps) * height);
        
        svg.append("text").text((step > 1 ? i * step : i / (1 / step)) + (unit ? unit : ""))
            .attr("class", "guide")
            .attr("x", -5)
            .attr("y", height - (i / steps) * height + 6);
    }
}

function mk_datum(svg, data) {
    var bar = svg.selectAll("g").data(data).enter();

    var g = bar.append("a")
        .attr("xlink:href", function(d) {
            return d.elt.querySelector("a").href;
        }).append("g").attr("class", "arrow");

    g.append("title")
        .text(function(d) {
            return print_date(d.time) + 
                "\nOn " + d.branch +
                "\nTook " + Math.round(d.speed * 10) / 10 + "m";
        });
    
    return g;
}

function make_speed_graph(node, data) {
    if (!data.length) return no_data(node);

    var spacing = width / data.length;
    
    var max = Math.max.apply(null, data.map(function(x) { return x.speed }));
    var step = step_size(max);
    var steps = max ? Math.ceil(max / step) : 0;
    var max = steps * step;

    var svg = initialize_svg(node);
    add_axes(svg);
    add_gridlines(svg, step, steps, "m");

    var g = mk_datum(svg, data);

    g.append("circle")
        .attr("fill", function(d) { return key(d.branch) })
        .attr("cx", function(d, i) { return (i + .5) * spacing })
        .attr("cy", function(d) { return height - height * d.speed / max })
        .attr("r", spacing * .75 / 2);

}

function select_data(data, options, tag) {
    return data = DATA.filter(function(x) {
        var out = true;
        for (var flag in options) {
            out = out && (x.flags.indexOf(flag) !== -1) == OPTIONS[flag];
        }
        return out && x.tag == tag;
    });
}

function render(node1, node2, data, options, tag) {
    node1.selectAll("*").remove();
    node2.selectAll("*").remove();
    // Update classes
    var olds = Array.prototype.slice.call(document.getElementsByClassName("selected"));
    olds.forEach(function(old) { old.classList.remove("selected") })
    document.getElementById("suite-" + tag).classList.add("selected");
    for (var flag in options) {
        if (options[flag]) document.getElementById("flag-" + flag).classList.add("selected");
    }
    var data = select_data(data, options, tag);
    make_accuracy_graph(node1, data, "bits");
    make_speed_graph(node2, data);
}

function draw_results(node1, node2) {
    DATA = get_data(document.getElementById("reports"));
    OPTIONS = {"rules:numerics": true};
    TAG = null;
    NODE1 = node1;
    NODE2 = node2;

    function toggle_tag(tag) {
        return function(evt) {
            TAG = tag;
            render(NODE1, NODE2, DATA, OPTIONS, TAG);
        }
    }

    function toggle_flag(flag) {
        return function(evt) {
            OPTIONS[flag] = !OPTIONS[flag];
            render(NODE1, NODE2, DATA, OPTIONS, TAG);
        }
    }

    var best_type = null;
    var type_list = document.getElementById("suites");
    for (var type in used_tag) {
        if (!type) continue;
        if ((!best_type || used_tag[type] > used_tag[best_type]) && type !== "tutorial") best_type = type;
        var li = document.createElement("li");
        var a = document.createElement("a");
        a.href = "#" + type;
        a.textContent = type;
        a.addEventListener("click", toggle_tag(type));
        li.id = "suite-" + type;
        li.appendChild(a);
        type_list.appendChild(li)
    }
    TAG = best_type;
    if (!TAG) return;

    var flag_list = document.getElementById("classes");
    for (var flag in OPTIONS) {
        var li = document.createElement("li");
        var a = document.createElement("a");
        a.href = "#" + flag;
        a.textContent = flag;
        a.addEventListener("click", toggle_flag(flag));
        li.id = "flag-" + flag;
        li.appendChild(a);
        if (OPTIONS[flag]) li.classList.add("selected");
        flag_list.appendChild(li)
    }

    var branches = [];
    var toclinks = document.getElementById("toc").querySelectorAll("li a");
    for (var i = 0; i < toclinks.length; i++) {
        branches.push(toclinks[i].textContent);
    }
    key = d3.scale.category20().domain(branches);

    render(NODE1, NODE2, DATA, OPTIONS, TAG);

    var branches = [];
    var toclinks = document.getElementById("toc").querySelectorAll("li a");
    for (var i = 0; i < toclinks.length; i++) {
        branches.push(toclinks[i].textContent);
    }
    key = d3.scale.category20().domain(branches);

    for (var i = 0; i < toclinks.length; i++) {
        if (used_branch[branches[i]]) {
            toclinks[i].style.color = key(branches[i]);
            toclinks[i].style.borderColor = key(branches[i]);
        }
    }
}
