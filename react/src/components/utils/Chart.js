import moment from 'moment';
import * as d3 from 'd3';

export const getLines = ({ xScale, yScale }) =>
  d3
    .line()
    .x(d => xScale(d.priceTime.toDate()))
    .y(d => yScale(d.close))
    .curve(d3.curveMonotoneX);

const xBand = xExtent =>
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

  const margin = { top: 10, right: 10, bottom: 15, left: 15 };
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

const dayFormatter = d => {
  return d3.timeFormat('%b %d')(d);
};

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

export const getBrush = ({ width, height, brushed }) => {
  d3.brushX()
    .extent([[0, 0], [width, height]])
    .on('brush end', brushed);
};

export const getZoom = ({ width, height, zoomed }) => {
  d3.zoom()
    .scaleExtent([1, Infinity])
    .translateExtent([[0, 0], [width, height]])
    .extent([[0, 0], [width, height]])
    .on('zoom', zoomed);
};
