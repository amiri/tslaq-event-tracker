import moment from 'moment';
import * as d3 from 'd3';

export const getXScale = ({ xExtent, width, margin }) =>
  d3
    .scaleBand()
    .domain(
      d3.timeDay
        .range(xExtent[0].toDate(), +xExtent[1].toDate() + 1)
        .filter(d => {
          const est = moment(d).tz('America/New_York');
          return est.dayOfYear(1) || (est.day() !== 0 && est.day() !== 6);
        }),
    )
    .range([margin.left, width - margin.right]);

export const getYScale = ({ yExtent, height, margin }) =>
  d3
    .scaleLinear()
    .domain([0, yExtent[1]])
    .range([height - margin.bottom, margin.top]);

// export const getMarginContext = (margin, dimensions) => {};

export const calculateDimensions = ({ height }) => {
  const totalHeightContext = Math.floor(height / 6);
  const totalHeightFocus = height - totalHeightContext;

  // console.log('totalHeightContext: ', totalHeightContext);
  // console.log('totalHeightFocus: ', totalHeightFocus);

  const margin = { top: 10, right: 10, bottom: 15, left: 15 };
  const heightContext = totalHeightContext - margin.top - margin.bottom;
  const heightFocus = totalHeightFocus - margin.top - margin.bottom;

  // console.log('heightContext: ', heightContext);
  // console.log('heightFocus: ', heightFocus);
  return {
    totalHeightContext,
    totalHeightFocus,
    margin,
    heightContext,
    heightFocus,
  };
};

export const getXAxis = (g, { xScale, tickVals, height, margin }) => {
  g.attr('transform', `translate(0,${height - margin.bottom})`)
    .call(
      d3
        .axisBottom(xScale)
        .tickValues(tickVals)
        .tickFormat(d =>
          d <= d3.timeYear(d)
            ? d3.timeFormat('%Y')(d)
            : d3
                .timeFormat('%b')(d)
                .charAt(0),
        ),
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

export const getTickVals = (xExtent) => {
    const duration = moment.duration(xExtent[1].diff(xExtent[0]));
    if (duration.asWeeks() < 4) {
        console.log(d3.timeDay.range(xExtent[0].toDate(), xExtent[1].toDate()).filter(d => d.getDay() == 1));
    }
    console.log(duration.humanize());
    const switchTicks = (dur) => {
        switch (dur) {
            case dur.asWeeks() < 4:
              return d3.timeDay
              .range(xExtent[0].toDate(), xExtent[1].toDate())
              .filter(d => d.getDay() == 1);
            case dur.asMonths() < 4:
              return d3.timeWeek
              .range(xExtent[0].toDate(), xExtent[1].toDate())
              .filter(d => d.getDate() == 1);
            case dur.asYears() > 1:
              return d3.timeMonth
              .range(xExtent[0].toDate(), xExtent[1].toDate())
              .filter(d => d.getMonth() % 3 === 0);
            default:
              return d3.timeMonth
              .range(xExtent[0].toDate(), xExtent[1].toDate())
              .filter(d => d.getMonth() % 3 === 0);
        }
    };
    const ret = switchTicks(duration);
    console.log(ret);
    return ret;
};
