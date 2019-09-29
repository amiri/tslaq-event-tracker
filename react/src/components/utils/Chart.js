import moment from 'moment';
import * as d3 from 'd3';

export const getLines = ({ xScale, yScale }) =>
  d3
    .line()
    .x(d => xScale(d.priceTime.toDate()))
    .y(d => yScale(d.close))
    .curve(d3.curveMonotoneX);

export const xBand = xExtent =>
  d3.timeDay.range(xExtent[0].toDate(), +xExtent[1].toDate() + 1).filter(d => {
    const est = moment(d).tz('America/New_York');
    return (
      est.dayOfYear() === 1 ||
      est.date() === 1 ||
      est.day() === 1 ||
      (est.day() !== 0 && est.day() !== 6)
    );
  });

export const getXScale = ({ xExtent, width, margin }) =>
  d3
    .scaleBand()
    .domain(xBand(xExtent))
    .range([margin.left, width - margin.right]);

export const getYScale = ({ yExtent, height, margin }) =>
  d3
    .scaleLinear()
    .domain([0, yExtent[1]])
    .range([height - margin.bottom, margin.top]);

export const calculateDimensions = ({ height }) => {
  const totalHeightContext = Math.floor(height / 6);
  const totalHeightFocus = height - totalHeightContext;

  const margin = { top: 10, right: 20, bottom: 15, left: 15 };
  const heightContext = totalHeightContext - margin.top - margin.bottom;
  const heightFocus = totalHeightFocus - margin.top - margin.bottom;

  return {
    totalHeightContext,
    totalHeightFocus,
    margin,
    heightContext,
    heightFocus,
  };
};

export const getXAxis = (g, { xScale, tickVals, tickFmt, height, margin }) => {
  g.attr('transform', `translate(0,${height - margin.bottom})`)
    .call(
      d3
        .axisBottom(xScale)
        .tickValues(tickVals)
        .tickFormat(tickFmt),
    )
    .call(g => g.select('.domain').remove());
};

export const getYAxis = (g, { yScale, margin, width }) => {
  g.attr('transform', `translate(${margin.left},0)`)
    .call(d3.axisRight(yScale).tickSize(width - margin.left - margin.right))
    .call(g => g.select('.domain').remove())
    .call(g =>
      g
        .selectAll('.tick:not(:first-of-type) line')
        .attr('stroke-opacity', 0.15)
        .attr('stroke-dasharray', '2,2'),
    )
    .call(g =>
      g
        .selectAll('.tick text')
        .attr('x', 4)
        .attr('dy', -4),
    );
};

const dayFormatter = d => d3.timeFormat('%b %d')(d);

const yearFormatter = d => {
  const fmt =
    d <= d3.timeYear(d)
      ? d3.timeFormat('%Y')(d)
      : d3
          .timeFormat('%b')(d)
          .charAt(0);
  return fmt;
};

export const getTickVals = ({ xExtent, timeZone }) => {
  const duration = moment.duration(xExtent[1].diff(xExtent[0]));
  const ticks =
    duration.asWeeks() < 4
      ? {
          tickVals: xBand(xExtent),
          tickFmt: dayFormatter,
        }
      : duration.asMonths() < 4
      ? {
          tickVals: xBand(xExtent).filter(
            d =>
              moment(d)
                .tz(timeZone)
                .day() === 1,
          ),
          tickFmt: dayFormatter,
        }
      : duration.asYears() > 1
      ? {
          tickVals: d3.timeMonth
            .range(xExtent[0].toDate(), xExtent[1].toDate())
            .filter(d => d.getMonth() % 3 === 0),
          tickFmt: yearFormatter,
        }
      : {
          tickVals: d3.timeMonth
            .range(xExtent[0].toDate(), xExtent[1].toDate())
            .filter(d => d.getMonth() % 3 === 0),
          tickFmt: yearFormatter,
        };
  return ticks;
};

export const getBrush = ({ width, height, brushed }) =>
  d3
    .brushX()
    .extent([[0, 0], [width, height]])
    .on('brush end', brushed);

export const getZoom = ({ width, height, zoomed }) =>
  d3
    .zoom()
    .scaleExtent([1, Infinity])
    .translateExtent([[0, 0], [width, height]])
    .extent([[0, 0], [width, height]])
    .on('zoom', zoomed);

// FocusLines Enter + Update + Remove
export const updateFocusLines = ({ s, xScale, yScale }) => {
  s.enter()
    .append('path')
    .attr('class', 'line')
    .merge(s)
    .attr('d', getLines({ xScale, yScale }));

  // FocusLines Exit
  s.exit().remove();
};

// ContextLines Enter + Update + Remove
export const updateContextLines = ({ s, xScale, yScale }) => {
  s.enter()
    .append('path')
    .attr('class', 'line')
    .merge(s)
    .attr('d', getLines({ xScale, yScale }));

  // ContextLines Exit
  s.exit().remove();
};
// FocusXAxis Enter + Update + Remove
export const updateFocusXAxis = ({
  s,
  getXAxis,
  xScale,
  tickVals,
  tickFmt,
  height,
  margin,
}) => {
  s.enter()
    .append('g')
    .attr('class', 'x-axis')
    .merge(s)
    .call(getXAxis, {
      xScale,
      tickVals,
      tickFmt,
      height,
      margin,
    });

  // FocusXAxis Exit
  s.exit().remove();
};

// ClipPath Enter + Update + Remove
export const updateClipPath = ({ s, width, height, margin }) => {
  s.enter()
    .append('defs')
    .append('clipPath')
    .attr('id', 'clip')
    .append('rect')
    .attr('width', width)
    .attr('height', height)
    .merge(s)
    .attr('transform', `translate(${margin.left},${margin.top})`)
    ;

  // ClipPath Exit
  s.exit().remove();
};

// FocusYAxis Enter + Update + Remove
export const updateFocusYAxis = ({ s, yScale, margin, width }) => {
  s.enter()
    .append('g')
    .attr('class', 'y-axis')
    .merge(s)
    .call(getYAxis, { yScale, margin, width });

  // FocusYAxis Exit
  s.exit().remove();
};

// ContextXAxis Enter + Update + Remove
export const updateContextXAxis = ({
  s,
  xScale,
  tickVals,
  tickFmt,
  height,
  margin,
}) => {
  s.enter()
    .append('g')
    .attr('class', 'x-axis')
    .merge(s)
    .call(getXAxis, {
      xScale,
      tickVals,
      tickFmt,
      height,
      margin,
    });

  // ContextXAxis Exit
  s.exit().remove();
};

export const updateContextBrush = ({ brush, s, xScale }) => {
  s.enter()
    .append('g')
    .attr('class', 'brush')
    .merge(s)
    .call(brush)
    .call(brush.move, xScale.range());
  s.exit().remove();
};

export const updateContextBrushInvert = ({ brush, s, t, xScale }) => {
  s.enter()
    .append('g')
    .attr('class', 'brush')
    .merge(s)
    .call(brush.move, xScale.range().map(t.invertX, t));
  s.exit().remove();
};

export const scaleBandInvert = scale => val => {
  console.log('Val: ', val);
  const domain = scale.domain();
  const paddingOuter = scale(domain[0]);
  const eachBand = scale.step();
  const index = Math.floor((val - paddingOuter) / eachBand);
  console.log('Index val: ', val, index);
  console.log(
    'Val Index val: ',
    val,
    index,
    domain[Math.max(0, Math.min(index, domain.length - 1))],
  );
  return domain[Math.max(0, Math.min(index, domain.length - 1))];
};

export const updateZoom = ({ s, width, height, zoom, margin }) => {
  s.enter()
    .append('rect')
    .attr('class', 'zoom')
    .merge(s)
    .attr('width', width)
    .attr('height', height)
    .attr('transform', `translate(${margin.left},${margin.top})`)
    .call(zoom);
};

export const transformZoom = ({ s, e, zoom, width }) => {
  s.select('.zoom').call(
    zoom.transform,
    d3.zoomIdentity.scale(width / (e[1] - e[0])).translate(-e[0], 0),
  );
};

export const getZoomF = ({
  width,
  xScale,
  focusLines,
  contextBrush,
  focusXAxis,
  yScale,
  tickVals,
  tickFmt,
  heightFocus,
  heightContext,
  margin,
  brush,
  brushed,
}) => () => {
  if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'brush') return; // ignore zoom-by-brush
  const t = d3.event.transform;
  xScale.domain(t.rescaleX(xScale).domain());
  updateFocusLines({ s: focusLines, xScale, yScale });
  updateFocusXAxis({
    s: focusXAxis,
    getXAxis,
    xScale,
    tickVals,
    tickFmt,
    height: heightFocus,
    margin,
  });
  updateContextBrushInvert({
    s: contextBrush,
    brush,
    width,
    height: heightContext,
    xScale,
    brushed,
    t,
  });
};

export const getBrushF = ({
  xScale,
  svg,
  focusLines,
  focusXAxis,
  yScale,
  tickVals,
  tickFmt,
  height,
  width,
  margin,
  zoom,
}) => () => {
  if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'zoom') return; // ignore brush-by-zoom
  const s = d3.event.selection || xScale.range();
  // console.log(xScale.domain);
  // console.log(s);
  xScale.domain(s.map(scaleBandInvert(xScale), xScale));
  updateFocusLines({ s: focusLines, xScale, yScale });
  updateFocusXAxis({
    s: focusXAxis,
    getXAxis,
    xScale,
    tickVals,
    tickFmt,
    height,
    margin,
  });
  transformZoom({ s: svg, e: s, zoom, height, width, margin });
};
