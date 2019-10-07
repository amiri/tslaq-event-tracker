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
    .scaleTime()
    .domain(xExtent)
    .range([margin.left, width - margin.right]);

export const getYScale = ({ yExtent, height, margin }) =>
  d3
    .scaleLinear()
    .domain([yExtent[0] - 5, yExtent[1]])
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
        // .selectAll('.tick:not(:first-of-type) line')
        .selectAll('.tick line')
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

export const updateContextBrush = ({ brush, s, xScale }) => {
  s.enter()
    .append('g')
    .attr('class', 'brush')
    .call(brush)
    .call(brush.move, xScale.range());
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
    .attr('transform', `translate(${margin.left},${margin.top})`);

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

export const updateZeroLine = ({ s, yScale, yExtent, width, margin }) => {
  s.enter()
    .append('line')
    .attr('class', 'zero')
    .merge(s)
    .attr('x1', margin.left)
    .attr('y1', yScale(yExtent[0] - 5))
    .attr('x2', width - margin.left - margin.right)
    .attr('y2', yScale(yExtent[0] - 5))
    .attr('stroke-width', 1)
    .attr('stroke', 'black');
};

export const draw = ({
  ps,
  width,
  height,
  margin,
  heightContext,
  heightFocus,
  timeZone,
  svgRef,
  contextRef,
  focusRef,
}) => {
  // Extents
  const xExtent = d3.extent(ps, p => p.priceTime);
  const yExtent = d3.extent(ps, p => p.high);

  // Scales
  var xScale = getXScale({ xExtent, width, margin });
  // var xScaleContext = getXScale({ xExtent, width, margin });

  const yScale = getYScale({ yExtent, height: heightFocus, margin });
  const yScaleContext = getYScale({
    yExtent,
    height: heightContext,
    margin,
  });

  // Tick values
  const { tickVals, tickFmt } = getTickVals({ xExtent, timeZone });

  // Size svg and g refs
  const svg = d3
    .select(svgRef.current)
    .attr('preserveAspectRatio', 'xMinYMin meet')
    .attr('viewBox', `0 0 ${width ? width : 0} ${height ? height : 0}`);

  const focus = d3
    .select(focusRef.current)
    .attr('transform', `translate(0, ${margin.top})`);

  const context = d3
    .select(contextRef.current)
    .attr(
      'transform',
      `translate(0, ${heightFocus + margin.bottom + margin.top})`,
    );

  // ClipPath
  const clipPath = svg.selectAll('defs').data(['dummy']);
  updateClipPath({ s: clipPath, width, height: heightFocus, margin });

  // ContextLines
  const contextLines = context.selectAll('.line').data([ps]);
  updateContextLines({
    s: contextLines,
    xScale,
    yScale: yScaleContext,
  });

  // FocusLines
  const focusLines = focus.selectAll('.line').data([ps]);
  updateFocusLines({ s: focusLines, xScale, yScale });

  // FocusXAxis
  const focusXAxis = focus.selectAll('.x-axis').data(['dummy']);
  updateFocusXAxis({
    s: focusXAxis,
    getXAxis,
    xScale,
    tickVals,
    tickFmt,
    height: heightFocus,
    margin,
  });

  // FocusYAxis
  const focusYAxis = focus.selectAll('.y-axis').data(['dummy']);
  updateFocusYAxis({ s: focusYAxis, margin, yScale, width });

  // ContextXAxis
  const contextXAxis = context.selectAll('.x-axis').data(['dummy']);
  updateContextXAxis({
    s: contextXAxis,
    xScale,
    tickVals,
    tickFmt,
    height: heightContext + margin.bottom,
    margin,
  });

  // ZeroLine
  const zeroLine = focus.selectAll('.zero').data(['dummy']);
  updateZeroLine({
    s: zeroLine,
    yScale,
    yExtent,
    width,
    margin,
  });
};
