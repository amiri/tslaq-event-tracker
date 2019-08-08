import React from 'react';
import { render } from 'react-dom';
import Api from './Api';
import PricesContextProvider from './contexts/PricesContext';
import EventsContextProvider from './contexts/EventsContext';
import AuthContextProvider from './contexts/AuthContext';
import NavBar from './components/NavBar';
import Chart from './components/Chart';
import './App.css';
import { Layout } from 'antd';

const { Header, Content } = Layout;

window.api = new Api();

const App = () => {
  return (
    // <React.StrictMode>
    <AuthContextProvider>
      <PricesContextProvider>
        <EventsContextProvider>
          <Layout>
            <Header>
              <NavBar />
            </Header>
            <Content>
              <Chart />
            </Content>
          </Layout>
        </EventsContextProvider>
      </PricesContextProvider>
    </AuthContextProvider>
    // </React.StrictMode>
  );
};

render(React.createElement(App), document.getElementById('root'));
