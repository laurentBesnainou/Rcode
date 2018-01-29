!function(t){function e(i){if(a[i])return a[i].exports;var r=a[i]={i:i,l:!1,exports:{}};return t[i].call(r.exports,r,r.exports,e),r.l=!0,r.exports}var a={};e.m=t,e.c=a,e.i=function(t){return t},e.d=function(t,a,i){e.o(t,a)||Object.defineProperty(t,a,{configurable:!1,enumerable:!0,get:i})},e.n=function(t){var a=t&&t.__esModule?function(){return t.default}:function(){return t};return e.d(a,"a",a),a},e.o=function(t,e){return Object.prototype.hasOwnProperty.call(t,e)},e.p="",e(e.s=4)}([function(t,e,a){"use strict";function i(t){return t&&t.__esModule?t:{default:t}}Object.defineProperty(e,"__esModule",{value:!0}),a(2);var r=a(1),n=i(r);e.default=Vizabi.Tool.extend("BarRankChart",{init:function(t,e){this.name="barrankchart",this.components=[{component:n.default,placeholder:".vzb-tool-viz",model:["state.time","state.entities","state.marker","locale","ui"]},{component:Vizabi.Component.get("timeslider"),placeholder:".vzb-tool-timeslider",model:["state.time","state.entities","state.marker","ui"]},{component:Vizabi.Component.get("dialogs"),placeholder:".vzb-tool-dialogs",model:["state","ui","locale"]},{component:Vizabi.Component.get("buttonlist"),placeholder:".vzb-tool-buttonlist",model:["state","ui","locale"]},{component:Vizabi.Component.get("treemenu"),placeholder:".vzb-tool-treemenu",model:["state.marker","state.marker_tags","state.time","locale"]},{component:Vizabi.Component.get("datanotes"),placeholder:".vzb-tool-datanotes",model:["state.marker","locale"]},{component:Vizabi.Component.get("datawarning"),placeholder:".vzb-tool-datawarning",model:["locale"]},{component:Vizabi.Component.get("steppedspeedslider"),placeholder:".vzb-tool-stepped-speed-slider",model:["state.time","locale"]}],this._super(t,e)},default_model:{state:{time:{autogenerate:{data:"data",conceptIndex:0,conceptType:"time"}},entities:{autogenerate:{data:"data",conceptIndex:0}},entities_colorlegend:{autogenerate:{data:"data",conceptIndex:0}},entities_tags:{},marker_tags:{space:["entities_tags"],label:{},hook_parent:{}},entities_allpossible:{autogenerate:{data:"data",conceptIndex:0}},marker_allpossible:{space:["entities_allpossible"],label:{use:"property",autogenerate:{conceptIndex:0}}},marker:{space:["entities","time"],axis_x:{use:"indicator",allow:{scales:["linear","log"]},autogenerate:{conceptIndex:0,conceptType:"measure"}},axis_y:{use:"property",allow:{scales:["ordinal","nominal"]},autogenerate:{conceptIndex:0}},label:{use:"property",autogenerate:{conceptIndex:0}},color:{syncModels:["marker_colorlegend"],autogenerate:{conceptIndex:0,conceptType:"entity_set"}}},marker_colorlegend:{space:["entities_colorlegend"],label:{use:"property",which:"name"},hook_rank:{use:"property",which:"rank"},hook_geoshape:{use:"property",which:"shape_lores_svg"}}},locale:{},ui:{chart:{},datawarning:{doubtDomain:[],doubtRange:[]},buttons:["colors","find","show","moreoptions","fullscreen","presentation"],dialogs:{popup:["timedisplay","colors","find","axes","show","moreoptions"],sidebar:["timedisplay","colors","find"],moreoptions:["opacity","speed","colors","presentation","about"]},presentation:!1}}})},function(t,e,a){"use strict";function i(t,e,a){return e in t?Object.defineProperty(t,e,{value:a,enumerable:!0,configurable:!0,writable:!0}):t[e]=a,t}Object.defineProperty(e,"__esModule",{value:!0});var r=function(){function t(t,e){var a=[],i=!0,r=!1,n=void 0;try{for(var o,s=t[Symbol.iterator]();!(i=(o=s.next()).done)&&(a.push(o.value),!e||a.length!==e);i=!0);}catch(t){r=!0,n=t}finally{try{!i&&s.return&&s.return()}finally{if(r)throw n}}return a}return function(e,a){if(Array.isArray(e))return e;if(Symbol.iterator in Object(e))return t(e,a);throw new TypeError("Invalid attempt to destructure non-iterable instance")}}(),n=Vizabi.utils,o=Vizabi.helpers["d3.axisWithLabelPicker"],s=Vizabi.iconset.question,l=Vizabi.iconset.warn,h="#333",c="#fdfdfd",d=Vizabi.Component.extend({init:function(t,e){var i=this;this.name="barrankchart-component",this.template=a(3),this.model_expects=[{name:"time",type:"time"},{name:"entities",type:"entities"},{name:"marker",type:"model"},{name:"locale",type:"locale"},{name:"ui",type:"ui"}],this.model_binds={"change:time.value":function(){i.model._ready&&i._readyOnce&&i.onTimeChange()},"change:marker.select":function(){i._readyOnce&&(i._selectBars(),i._updateOpacity(),i._updateDoubtOpacity(),i._scroll())},"change:marker.axis_x.scaleType":function(){i._readyOnce&&i.loadData()&&i.draw(!0)},"change:marker.color.palette":function(){i._drawColors()},"change:marker.highlight":function(){i._updateOpacity()},"change:marker.opacitySelectDim":function(){i._updateOpacity()},"change:marker.opacityRegular":function(){i._updateOpacity()}},this._super(t,e),this.xScale=null,this.cScale=d3.scaleOrdinal(d3.schemeCategory10),this.xAxis=o("bottom")},onTimeChange:function(){var t=this;this.model.marker.getFrame(this.model.time.value,function(e){t.values=e,t.values&&t.loadData()&&t.draw()})},readyOnce:function(){this.element=d3.select(this.element),this.header=this.element.select(".vzb-br-header"),this.infoEl=this.element.select(".vzb-br-axis-info"),this.barViewport=this.element.select(".vzb-br-barsviewport"),this.barSvg=this.element.select(".vzb-br-bars-svg"),this.barContainer=this.element.select(".vzb-br-bars"),this.dataWarningEl=this.element.select(".vzb-data-warning"),this.wScale=d3.scale.linear().domain(this.model.ui.datawarning.doubtDomain).range(this.model.ui.datawarning.doubtRange),this.xAxis.tickFormat(this.model.marker.axis_x.getTickFormatter()),this._localeId=this.model.locale.id,this._entityLabels={},this._presentation=!this.model.ui.presentation,this._formatter=this.model.marker.axis_x.getTickFormatter(),this.ready(),this._selectBars()},ready:function(){var t=this;this.model.marker.getFrame(this.model.time.value,function(e){t.values=e,t.values&&t.loadData()&&(t.draw(!0),t._updateOpacity(),t._drawColors())})},resize:function(){this.draw(!0),this._drawColors()},loadData:function(){var t=this,e=this;this.translator=this.model.locale.getTFunction();var a=this.values.axis_x;return!!Object.keys(a).length&&(this.sortedEntities=this._sortByIndicator(a),this.header.select(".vzb-br-title").select("text").on("click",function(){return t.parent.findChildByName("gapminder-treemenu").markerID("axis_x").alignX("left").alignY("top").updateView().toggle()}),this.xScale=this.model.marker.axis_x.getScale(),this.cScale=this.model.marker.color.getScale(),n.setIcon(this.dataWarningEl,l).select("svg").attr("width",0).attr("height",0),this.dataWarningEl.append("text").text(this.translator("hints/dataWarning")),this.dataWarningEl.on("click",function(){return t.parent.findChildByName("gapminder-datawarning").toggle()}).on("mouseover",function(){return t._updateDoubtOpacity(1)}).on("mouseout",function(){return t._updateDoubtOpacity()}),n.setIcon(this.infoEl,s).select("svg").attr("width",0).attr("height",0),this.infoEl.on("click",function(){t.parent.findChildByName("gapminder-datanotes").pin()}),this.infoEl.on("mouseover",function(){var t=this.getBBox(),a=n.makeAbsoluteContext(this,this.farthestViewportElement),i=a(t.x-10,t.y+t.height+10);e.parent.findChildByName("gapminder-datanotes").setHook("axis_y").show().setPos(i.x,i.y)}),this.infoEl.on("mouseout",function(){e.parent.findChildByName("gapminder-datanotes").hide()}),!0)},draw:function(){var t=arguments.length>0&&void 0!==arguments[0]&&arguments[0];this.time_1=null==this.time?this.model.time.value:this.time,this.time=this.model.time.value;var e=this.model.time.playing&&this.time-this.time_1>0?this.model.time.delayAnimations:0;this.drawAxes(e,t)||this.drawData(e,t)},drawAxes:function(){var t=arguments.length>0&&void 0!==arguments[0]?arguments[0]:0,e={small:{margin:{top:60,right:5,left:5,bottom:20},headerMargin:{top:10,right:20,bottom:20,left:20},infoElHeight:16,infoElMargin:5,barHeight:18,barMargin:3,barRectMargin:5,barValueMargin:5,scrollMargin:20},medium:{margin:{top:60,right:5,left:5,bottom:20},headerMargin:{top:10,right:20,bottom:20,left:20},infoElHeight:16,infoElMargin:5,barHeight:21,barMargin:3,barRectMargin:5,barValueMargin:5,scrollMargin:25},large:{margin:{top:60,right:5,left:5,bottom:20},headerMargin:{top:10,right:20,bottom:20,left:20},infoElHeight:16,infoElMargin:5,barHeight:28,barMargin:4,barRectMargin:5,barValueMargin:5,scrollMargin:25}},a={medium:{margin:{top:60,right:10,left:10,bottom:40},headerMargin:{top:10,right:20,bottom:20,left:20},infoElHeight:25,infoElMargin:10,barHeight:25,barMargin:6},large:{margin:{top:60,right:10,left:10,bottom:40},headerMargin:{top:10,right:20,bottom:20,left:20},infoElHeight:16,infoElMargin:10,barHeight:30,barMargin:6}};this.activeProfile=this.getActiveProfile(e,a);var i=this.activeProfile,r=i.margin,o=i.headerMargin,s=i.infoElHeight,l=i.infoElMargin;if(this.height=parseInt(this.element.style("height"),10)||0,this.width=parseInt(this.element.style("width"),10)||0,!this.height||!this.width)return n.warn("Dialog resize() abort: vizabi container is too little or has display:none");this.barViewport.style("height",this.height-r.bottom-r.top+"px"),this.header.attr("height",r.top);var h=this.header.select(".vzb-br-title"),c=this.model.marker.axis_x.getConceptprops(),d=c.name,g=c.unit,u=h.select("text");if(g){u.text(d+", "+g);o.left+h.node().getBBox().width+l+s>this.width-o.right&&u.text(d)}else u.text(d);var m=h.node().getBBox(),b=o.left,p=o.top+m.height;h.attr("transform","translate("+b+", "+p+")");var f=this.infoEl;f.select("svg").attr("width",s+"px").attr("height",s+"px");var v=b+h.node().getBBox().width+l,x=o.top+s/4;f.attr("transform","translate("+v+", "+x+")");var y=this.header.select(".vzb-br-total");t?y.select("text").transition("text").delay(t).text(this.model.time.formatDate(this.time)):y.select("text").interrupt().text(this.model.time.formatDate(this.time)),y.style("opacity",Number("large"!==this.getLayoutProfile()));var _=y.node().getBBox(),w=this.width-o.right-_.width,z=o.top+_.height;y.attr("transform","translate("+w+", "+z+")").classed("vzb-transparent",m.width+_.width+10>this.width),this.element.select(".vzb-data-warning-svg").style("height",r.bottom+"px");var k=this.dataWarningEl.select("text").node().getBBox();this.dataWarningEl.attr("transform","translate("+(this.width-r.right-k.width)+", "+k.height+")").select("text"),this.dataWarningEl.select("svg").attr("width",k.height).attr("height",k.height).attr("x",-k.height-5).attr("y",1-k.height),this._updateDoubtOpacity()},drawData:function(){var t=this,e=arguments.length>0&&void 0!==arguments[0]?arguments[0]:0,a=arguments.length>1&&void 0!==arguments[1]&&arguments[1];this._createAndDeleteBars(this.barContainer.selectAll(".vzb-br-bar").data(this.sortedEntities,function(t){return t.entity}));var i=this.model.ui.presentation,r=this._presentation!==i;r&&(this._presentation=i);var n=void 0===this._entitiesCount||this._entitiesCount!==this.sortedEntities.length;(r||n)&&n&&(this._entitiesCount=this.sortedEntities.length),this._resizeSvg(),this._scroll(e);var o=this.activeProfile,s=o.barRectMargin,l=o.barValueMargin,h=o.scrollMargin,c=o.margin,d=this.model.marker.axis_x,g=d.getLimits(d.which),u=Math.abs(g.max)>=Math.abs(g.min),m=this.width-c.right-c.left-this._getWidestLabelWidth()-s-h;this.xScale.range([0,m]);var b=this.model.marker.axis_x.scaleType,p=("log"===b?0:this.xScale(0))||0,f=this._getWidestLabelWidth();p>(u?c.left:c.right)+this._getWidestLabelWidth()&&(f=p),p<0&&(this.xScale.range([0,m-Math.abs(p)]),p=("log"===b?0:this.xScale(0))||0);var v=function(e){return t.xScale(e)-p},x=u?"end":"start",y=u?"start":"end",_=u?c.left+f:this.width-f-h-c.right,w=u?_+s:_-s,z=u?w+l:w-l,k=this._getWidestLabelWidth(!0)+(u?c.left:c.right)<f;this.sortedEntities.forEach(function(i){var n=i.value;if((a||r||i.isNew)&&(i.barLabel.attr("x",_).attr("y",t.activeProfile.barHeight/2).attr("text-anchor",x).text(k?i.labelFull:i.labelSmall),i.barRect.attr("rx",t.activeProfile.barHeight/4).attr("ry",t.activeProfile.barHeight/4).attr("height",t.activeProfile.barHeight),i.barValue.attr("x",z).attr("y",t.activeProfile.barHeight/2).attr("text-anchor",y)),a||i.changedWidth||r){var o=Math.max(0,n&&v(Math.abs(n)));(a||i.changedWidth||r)&&i.barRect.transition().duration(e).ease(d3.easeLinear).attr("width",o),i.barRect.attr("x",w-(n<0?o:0)),(a||i.changedValue)&&i.barValue.text(t._formatter(n)||t.translator("hints/nodata"))}(a||i.changedIndex||r)&&i.self.transition().duration(e).ease(d3.easeLinear).attr("transform","translate(0, "+t._getBarPosition(i.index)+")")})},_resizeSvg:function(){var t=this.activeProfile,e=t.barHeight,a=t.barMargin;this.barSvg.attr("height",(e+a)*this.sortedEntities.length+"px")},_scroll:function(){var t=arguments.length>0&&void 0!==arguments[0]?arguments[0]:0,e=this.barContainer.select(".vzb-selected");if(!e.empty()){var a=e.datum(),i=this._getBarPosition(a.index),r=this.activeProfile.margin,n=this.height-r.top-r.bottom,o=i-(n+this.activeProfile.barHeight)/2;this.barViewport.transition().duration(t).tween("scrollfor"+a.entity,this._scrollTopTween(o))}},_createAndDeleteBars:function(t){var e=this,a=r(this.sortedEntities,1),i=a[0];this._entityLabels[i.entity]||(this._entityLabels[i.entity]=i.label);var n=this._entityLabels[i.entity]!==this.values.label[i.entity]&&this.model.locale.id!==this._localeId;n&&(this._localeId=this.model.locale.id,this._entityLabels[i.entity]=this.values.label[i.entity]),t.exit().remove(),t=(n?t:t.enter().append("g")).each(function(t){var a=d3.select(this),i=e.values.label[t.entity],r=i.length<12?i:i.substring(0,9)+"...";t.barLabel&&t.barLabel.remove();var o=a.append("text").attr("class","vzb-br-label").attr("dy",".325em"),s=o.text(i).node().getBBox().width,l=o.text(r).node().getBBox().width;if(Object.assign(t,{labelFullWidth:s,labelSmallWidth:l,labelFull:i,labelSmall:r,barLabel:o}),!n){a.attr("class","vzb-br-bar").classed("vzb-selected",e.model.marker.isSelected(t)).attr("id","vzb-br-bar-"+t.entity+"-"+e._id).on("mousemove",function(t){return e.model.marker.highlightMarker(t)}).on("mouseout",function(){return e.model.marker.clearHighlighted()}).on("click",function(t){e.model.marker.selectMarker(t)});var h=a.append("rect").attr("stroke","transparent"),c=a.append("text").attr("class","vzb-br-value").attr("dy",".325em");Object.assign(t,{self:a,isNew:!0,barRect:h,barValue:c})}}).merge(t)},_getWidestLabelWidth:function(){var t=arguments.length>0&&void 0!==arguments[0]&&arguments[0],e=t?"labelFullWidth":"labelSmallWidth",a=t?"labelFull":"labelSmall",i=this.sortedEntities.reduce(function(t,a){return t[e]<a[e]?a:t}),r=i.barLabel.text(),n=i.barLabel.text(i[a]).node().getBBox().width;return i.barLabel.text(r),n},_drawColors:function(){var t=this,e=this;this.barContainer.selectAll(".vzb-br-bar>rect").each(function(t){var a=t.entity,i=d3.select(this),r=e.values.color[a];r||0===r?i.style("fill",e._getColor(r)).attr("stroke","transparent"):i.style("fill",c).attr("stroke",h)}),this.barContainer.selectAll(".vzb-br-bar>text").style("fill",function(e){var a=e.entity;return t._getDarkerColor(t.values.color[a])})},_getColor:function(t){return d3.rgb(this.cScale(t))},_getDarkerColor:function(t){return this._getColor(t).darker(2)},_scrollTopTween:function(t){return function(){var e=this,a=d3.interpolateNumber(this.scrollTop,t);return function(t){e.scrollTop=a(t)}}},_getBarPosition:function(t){return(this.activeProfile.barHeight+this.activeProfile.barMargin)*t},_entities:{},_sortByIndicator:function(t){var e=this;return Object.keys(t).map(function(a){var r,n=e._entities[a],o=t[a],s=e.values.label[a],l=e._formatter(o);return n?Object.assign(n,{value:o,label:s,formattedValue:l,changedValue:l!==n.formattedValue,changedWidth:o!==n.value,isNew:!1}):e._entities[a]=(r={entity:a,value:o,formattedValue:l},i(r,e.model.entities.dim,a),i(r,"changedValue",!0),i(r,"changedWidth",!0),i(r,"isNew",!0),r)}).sort(function(t,e){var a=t.value;return e.value-a}).map(function(t,e){return Object.assign(t,{index:e,changedIndex:e!==t.index})})},_selectBars:function(){var t=this,e=this.model.entities.dim,a=this.model.marker.select;this.barContainer.classed("vzb-dimmed-selected",!1),this.barContainer.selectAll(".vzb-br-bar.vzb-selected").classed("vzb-selected",!1),a.length&&(this.barContainer.classed("vzb-dimmed-selected",!0),a.forEach(function(a){t.barContainer.select("#vzb-br-bar-"+a[e]+"-"+t._id).classed("vzb-selected",!0)}))},_updateOpacity:function(){var t=this.model.marker,e=t.highlight,a=t.select,i=t.opacityHighlightDim,r=t.opacitySelectDim,n=t.opacityRegular,o=e.length>0,s=a.length>0;this.barContainer.selectAll(".vzb-br-bar").style("opacity",function(e){return o&&t.isHighlighted(e)?1:s?t.isSelected(e)?n:r:o?i:n})},_updateDoubtOpacity:function(t){this.dataWarningEl.style("opacity",t||(this.model.marker.select.length?1:this.wScale(+this.model.time.value.getUTCFullYear().toString())))}});e.default=d},function(t,e){},function(t,e){t.exports='<!-- Bar Chart Component -->\n<div class="vzb-barrankchart">\n  <svg class="vzb-br-header">\n    <g class="vzb-br-title">\n      <text></text>\n    </g>\n    <g class="vzb-br-total">\n      <text></text>\n    </g>\n    <g class="vzb-br-axis-info vzb-noexport"></g>\n  </svg>\n\n  <div class="vzb-br-barsviewport vzb-dialog-scrollable">\n    <svg class="vzb-br-bars-svg">\n      <g class="vzb-br-bars"></g>\n    </svg>\n  </div>\n\n  <svg class="vzb-data-warning-svg">\n    <g class="vzb-data-warning vzb-noexport">\n      <svg></svg>\n      <text></text>\n    </g>\n  </svg>\n</div>\n'},function(t,e,a){t.exports=a(0)}]);
//# sourceMappingURL=barrankchart.js.map