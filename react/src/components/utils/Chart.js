import moment from 'moment';
import * as d3 from 'd3';
import SimpleCrypto from 'simple-crypto-js';

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

export const getLines = ({ xScale, yScale }) =>
  d3
    .line()
    .x(d => xScale(d.priceTime.toDate()))
    .y(d => yScale(d.close))
    .curve(d3.curveMonotoneX);

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
    .extent([
      [0, 0],
      [width, height],
    ])
    .on('brush end', brushed);

export const getZoom = ({ width, height, zoomed }) =>
  d3
    .zoom()
    .scaleExtent([1, Infinity])
    .translateExtent([
      [0, 0],
      [width, height],
    ])
    .extent([
      [0, 0],
      [width, height],
    ])
    .on('zoom', zoomed);

export const updateContextBrush = ({ brush, s, xScale }) => {
  s.enter()
    .append('g')
    .attr('class', 'brush')
    .call(brush)
    .call(brush.move, xScale.range());
  s.exit().remove();
};

// XAxis Enter + Update + Remove
export const updateXAxis = ({
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

  // XAxis Exit
  s.exit().remove();
};

// ClipPath Enter + Update + Remove
export const updateClipPath = ({ s, width, height, margin }) => {
  s.enter()
    .append('defs')
    .append('clipPath')
    .attr('id', 'clip')
    .append('rect')
    .attr('width', width - margin.left - margin.right)
    .attr('height', height)
    .merge(s)
    .attr('transform', `translate(${margin.left},${margin.top})`);

  // ClipPath Exit
  s.exit().remove();
};

// FocusYAxis Enter + Update + Remove
export const updateYAxis = ({ s, yScale, margin, width }) => {
  s.enter()
    .append('g')
    .attr('class', 'y-axis')
    .merge(s)
    .call(getYAxis, { yScale, margin, width });

  // FocusYAxis Exit
  s.exit().remove();
};

export const updateZeroLine = ({ s, yScale, yExtent, width, margin }) => {
  s.enter()
    .append('line')
    .attr('class', 'zero')
    .merge(s)
    .attr('x1', margin.left)
    .attr('y1', yScale(yExtent[0] - 5))
    .attr('x2', width - margin.right)
    .attr('y2', yScale(yExtent[0] - 5))
    .attr('stroke-width', 1)
    .attr('stroke', 'black');
};

export const updateHighLine = ({ s, yScale, yExtent, width, margin }) => {
  s.enter()
    .append('line')
    .attr('class', 'high')
    .merge(s)
    .attr('x1', margin.left)
    .attr('y1', yScale(yExtent[1]))
    .attr('x2', width - margin.right)
    .attr('y2', yScale(yExtent[1]))
    .attr('stroke-width', '0.5px')
    .attr('stroke', '#85bb65');
};

export const updateLowLine = ({ s, yScale, yExtent, width, margin }) => {
  s.enter()
    .append('line')
    .attr('class', 'low')
    .merge(s)
    .attr('x1', margin.left)
    .attr('y1', yScale(yExtent[0]))
    .attr('x2', width - margin.right)
    .attr('y2', yScale(yExtent[0]))
    .attr('stroke-width', '0.25px')
    .attr('stroke', '#8A0707');
};

export const isSelected = ({ coords, cx, cy }) => {
  const x0 = coords[0][0],
    x1 = coords[1][0],
    y0 = coords[0][1],
    y1 = coords[1][1];
  return x0 <= cx && cx <= x1 && y0 <= cy && cy <= y1;
};

const encryptionSecret = 'bk';

export const crypto = new SimpleCrypto(encryptionSecret);

export const openViewModal = ({ id, history }) => {
  history.push({
    pathname: '/event/',
    search: `?id=${id}`,
    state: { visible: true },
  });
};

export const openNewEventModal = ({ eventDate }) => {
  console.log(eventDate);
};

export const encryptIds = ({ ids }) =>
  Buffer.from(crypto.encrypt(JSON.stringify(ids))).toString('base64');

export const decryptIds = ({ ids }) =>
  JSON.parse(crypto.decrypt(Buffer.from(ids, 'base64').toString()));
