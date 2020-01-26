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
    const s = d3.event.selection || xScale.range();
    brushF({
      range: s,
      xScale,
      eventType: d3.event.sourceEvent ? d3.event.sourceEvent.type : null,
    });
  }

  const brush = getBrush({ width, height: height + 5, brushed });

  const contextRef = useRef(null);
  const { timeZone } = config;

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
    contextBrush.call(brush).call(brush.move, xScale.range());
  }, [height, width]);

  // Brush
  useEffect(() => {
    if (brushDomain[0] >= 0 && brushDomain[1] >= 0) {
      const context = d3.select(contextRef.current);
      const contextBrush = context.selectAll('.brush');
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
