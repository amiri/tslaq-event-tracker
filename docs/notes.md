# Backend api for a React/d3-driven event tracker

## Entities
There are two major entities here.

- prices
These have dates.

- events
These have datetimes.

## Description

Prices are served from s3/cloudfront.

Events are served by this applicatiEvents are inserted along the price chart.

This application

1. serves the cloudfront s3 url for the price data,

2. serves the data for the events, and

3. allows creation of events.

## Tools

- React.js

https://blog.logrocket.com/data-visualization-in-react-using-react-d3-c35835af16d0

- d3.js

https://blog.logrocket.com/data-visualization-in-react-using-react-d3-c35835af16d0

- draft.js:

https://draftjs.org/

- draft.js image plugin

https://www.draft-js-plugins.com/plugin/image

# React app

`Chart` needs to be divided into more than 1 component. It will render html that in turn renders 2 svgs: context and focus.

Move spin into Focus and Context, each.

## src/components/Chart.js

- calculates dimensions of `Focus` and `Context`

- calculates filtered prices/events list based on config from `ChartContext`.

- onZoom sets state

- onBrush sets state

- zoomTransform is a function that brushed calls to modify the zoom.

- moveBrush is a function that zoomed calls to modify the brush.

`Chart` can be the main component. It includes `Focus` and `Context`.

## src/components/Focus.js

Renders candlesticks of the selected price/event domain.

Renders a zoom.

## src/components/Context.js

Renders line chart of the entire price/event domain.

Renders a brush.

## src/components/Brush.js

## src/components/Zoom.js
