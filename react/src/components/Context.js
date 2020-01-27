import React, { useRef, useEffect } from 'react';
import * as d3 from 'd3';
import {
  getXScale,
  getYScale,
  getLines,
  getTickVals,
  getBrush,
  updateXAxis,
  margin,
  getInitialSelection,
} from './utils/Chart';

const Context = ({ config, width, height, ps, brushF, brushDomain }) => {
  // Extents
  const xExtent = d3.extent(ps, p => p.priceTime);
  const yExtent = d3.extent(ps, p => p.high);

  // Scales
  const xScale = getXScale({ xExtent, width });
  const yScale = getYScale({ yExtent, height });

  // Brush
  function brushed() {
    if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'zoom') return; // ignore brush-by-zoom
    // console.log(
    //   'brushed eventType:',
    //   d3.event.sourceEvent ? d3.event.sourceEvent.type : 'NULL',
    // );
    // console.log('brushed selection:', d3.event.selection);
    // console.log('brushed xScale range:', xScale.range());
    const xRange = xScale.range();

    const s = d3.event.selection || xScale.range();
    const sel = [
      s[0] < xRange[0] ? xRange[0] : s[0],
      s[1] > xRange[1] ? xRange[1] : s[1],
    ];
    brushF({
      range: sel,
      xScale,
      eventType: d3.event.sourceEvent ? d3.event.sourceEvent.type : null,
    });
  }

  const brush = getBrush({ width, height: height + 5, brushed });

  const contextRef = useRef(null);
  const { timeZone, dateRange } = config;

  useEffect(() => {
    const context = d3.select(contextRef.current);
    const { tickVals, tickFmt } = getTickVals({ xExtent, timeZone });
    const contextXAxis = context.selectAll('.x-axis').data([0]);
    updateXAxis({
      s: contextXAxis,
      xScale,
      tickVals,
      tickFmt,
      height,
    });
    const contextBrush = context.selectAll('.brush').data([0]);
    const initialSelection = getInitialSelection({ xScale, dateRange });
    //console.log('In main Context effect moving brush to ', initialSelection);
    contextBrush.call(brush).call(brush.move, initialSelection);
  }, [height, width, dateRange]);

  // Brush
  useEffect(() => {
    if (brushDomain[0] >= 0 && brushDomain[1] >= 0) {
      const context = d3.select(contextRef.current);
      const contextBrush = context.selectAll('.brush');
      //console.log('In brush Context effect moving brush to ', brushDomain);
      //console.log('In brush Context effect xScale.range() is ', xScale.range());
      contextBrush.call(brush.move, brushDomain);
    }
  }, [brushDomain]);

  return (
    <svg
      className='context-svg'
      preserveAspectRatio='xMinYMin meet'
      viewBox={`0 0 ${width ? width : 0} ${height ? height + 5 : 0}`}
      transform={`translate(0, ${margin.top})`}
    >
      <g className='context' ref={contextRef}>
        <path className='line' d={getLines({ xScale, yScale })(ps)} />
        <g className='brush' />
      </g>
    </svg>
  );
};

export default Context;
