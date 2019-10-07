import React, { useRef, useEffect } from 'react';
import * as d3 from 'd3';
import {
  getXScale,
  getYScale,
  getLines,
  getTickVals,
  getBrush,
  updateContextXAxis,
} from './utils/Chart';

const Context = ({
  config,
  width,
  height,
  focusHeight,
  margin,
  ps,
  brushFn,
}) => {
  // Extents
  const xExtent = d3.extent(ps, p => p.priceTime);
  const yExtent = d3.extent(ps, p => p.high);

  // Scales
  const xScale = getXScale({ xExtent, width, margin });
  const yScale = getYScale({ yExtent, height, margin });

  // Brush
  function brushed() {
    if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'zoom') return; // ignore brush-by-zoom
    const s = d3.event.selection || xScale.range();
    brushFn({ range: s, xScale });
  }
  const brush = getBrush({ width, height, brushed });

  const contextRef = useRef(null);
  const { timeZone } = config;

  useEffect(() => {
    const context = d3.select(contextRef.current);
    const { tickVals, tickFmt } = getTickVals({ xExtent, timeZone });
    const contextXAxis = context.selectAll('.x-axis').data([0]);
    updateContextXAxis({
      s: contextXAxis,
      xScale,
      tickVals,
      tickFmt,
      height,
      margin,
    });
    const contextBrush = context.selectAll('.brush').data([0]);
    contextBrush.call(brush).call(brush.move, xScale.range());
  }, [contextRef, height]);

  return (
    <svg
      className='context-svg'
      preserveAspectRatio='xMinYMin meet'
      viewBox={`0 0 ${width ? width : 0} ${height ? height : 0}`}
      transform={`translate(0, ${focusHeight})`}
    >
      <g className='context' ref={contextRef}>
        <path className='line' d={getLines({ xScale, yScale })(ps)} />
        <g className='brush' />
      </g>
    </svg>
  );
};

export default Context;
