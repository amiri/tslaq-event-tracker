import React from 'react';
import { render } from 'react-dom';
import Api from './Api';
import PricesContextProvider from './contexts/PricesContext';
import EventsContextProvider from './contexts/EventsContext';
import AuthContextProvider from './contexts/AuthContext';

window.api = new Api();

const App = () => {
  return (
    <React.StrictMode>
      <AuthContextProvider>
        <PricesContextProvider>
          <EventsContextProvider>
            <p>Hello</p>
          </EventsContextProvider>
        </PricesContextProvider>
      </AuthContextProvider>
    </React.StrictMode>
  );
};

render(React.createElement(App), document.getElementById('root'));
