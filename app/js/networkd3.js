// general structure of custom JS was adapted from https://github.com/christophergandrud/networkD3

function(el, x) { 
	var link = d3.selectAll(".link")
	link.style("opacity", 0);
	var node = d3.selectAll(".node")
	node.select("text")
		.attr("x", function(d) { return Math.sqrt(d.nodesize)/2; });

	var g = d3.select('g')
	var height = d3.select('svg').attr('height');
	var width = d3.select('svg').attr('width');

	var options = {
		opacity: 1,
		clickTextSize: 10,
		opacityNoHover: 0.1,
		radiusCalculation: "Math.sqrt(d.nodesize)+6"
	}

	var unfocusDivisor = 4;

	var links = HTMLWidgets.dataframeToD3(x.links);
	var nodes = HTMLWidgets.dataframeToD3(x.nodes);
	var nodeIndex = {};
	var linkedByIndex = {};

	for (i=0; i<nodes.length; i++) {
		nodeIndex[nodes[i]['name']] = i
		nodes[i]['index'] = i
	}

	links.forEach(function(d) {
		linkedByIndex[d.source + "," + d.target] = 1;
		linkedByIndex[d.target + "," + d.source] = 1;
	});

	function neighboring(a, b) {
		return linkedByIndex[a.index + "," + b.index];
	}

	function nodeSize(d) {
		return eval(options.radiusCalculation);
	}

	function nodemouseover(d) {
		var unfocusDivisor = 4;
		link.transition().duration(200)
			.style("opacity", function(l) { return d != l.source && d != l.target ? 0 : 0.60 });

		node.transition().duration(200)
			.style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? +options.opacity : +options.opacity / unfocusDivisor; });
		d3.select(this).select("circle").transition()
			.duration(250)
			.attr("r", function(d){return nodeSize(d) + 3;});

		d3.select(this).select("text").transition()
			.duration(250)
			.attr("x", nodeSize(d))
			.style("stroke-width", ".5px")
			.style("font", options.clickTextSize + "px ")
			.style("opacity", 1);
	}

	function nodemouseout() {
		node.style("opacity", +options.opacity);
		link.transition()
			.duration(250)
			.style("opacity", 0);

		d3.select(this).select("circle").transition()
			.duration(250)
			.attr("r", function(d){return nodeSize(d);});
		node.select("text").transition()
			.duration(250)
			.attr("x", function(d) { return Math.sqrt(d.nodesize)/2; })
			.style("font", options.fontSize + "px ")
			.style("opacity", 1);
	}
	// function suggested by @cjyetmen on SO: https://stackoverflow.com/questions/44110370/implementing-tooltip-for-networkd3-app/44134845#44134845
	function nodemouseclick(d) {
		d3.selectAll(".xtooltip").remove(); 
		d3.select("body").append("div")
			.attr("class", "xtooltip")
			.style("position", "absolute")
			.style("border", "1px solid #999")
			.style("border-radius", "3px")
			.style("padding", "5px")
			.style("opacity", "0.85")
			.style("background-color", "#181818")
			.style("font-family", "sans-serif")
			.style("box-shadow", "2px 2px 6px #888888")
			.html("<a href=\"" + d.amboss + "\" target=\"_blank\">amboss.space</a>")
			.style("left", (d3.event.pageX) + "px")
			.style("top", (d3.event.pageY - 28) + "px");
		d3.event.stopPropagation()
	}

	d3.selectAll(".node")
		.on("mouseover", nodemouseover)
		.on("mouseout", nodemouseout)
		.on("click", nodemouseclick);
	d3.selectAll(".link")
		.on("mouseover", function(o) {d3.select(this).style("opacity", 0)})
		.on("mouseout", function(o) {d3.select(this).style("opacity", 0)});
	d3.select("body").on("click", function() {
		d3.selectAll(".xtooltip").remove(); 
	});
}
