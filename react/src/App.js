import React from 'react';
import { render } from 'react-dom';
import Api from './Api';
import PricesContextProvider from './contexts/PricesContext';
import EventsContextProvider from './contexts/EventsContext';
import AuthContextProvider from './contexts/AuthContext';
import ChartContextProvider from './contexts/ChartContext';
import NewEventModalContextProvider from './contexts/NewEventModalContext';
import NewCategoryModalContextProvider from './contexts/NewCategoryModalContext';
import NavBar from './components/NavBar';
import Chart from './components/Chart';
import NewEvent from './components/NewEvent';
import NewCategory from './components/NewCategory';
import './App.css';
import { Layout } from 'antd';
import { BrowserRouter as Router, Route } from 'react-router-dom';

const { Header, Content } = Layout;

window.api = new Api();

const App = () => {
  return (
    // <React.StrictMode>
    <AuthContextProvider>
      <PricesContextProvider>
        <EventsContextProvider>
          <ChartContextProvider>
            <NewEventModalContextProvider>
              <NewCategoryModalContextProvider>
                <Router>
                  <Layout style={{ height: '100%', width: '100%' }}>
                    <Header style={{ backgroundColor: '#f0f2f5' }}>
                      <Route path='/' component={NavBar} />
                    </Header>
                    <Content style={{ height: '100%', width: '100%' }}>
                      <Route path='/' component={Chart} />
                      <Route
                        path='/new'
                        render={props => <NewEvent {...props} />}
                      />
                      <Route
                        path='/new/category'
                        render={props => <NewCategory {...props} />}
                      />
                    </Content>
                  </Layout>
                </Router>
              </NewCategoryModalContextProvider>
            </NewEventModalContextProvider>
          </ChartContextProvider>
        </EventsContextProvider>
      </PricesContextProvider>
    </AuthContextProvider>
    // </React.StrictMode>
  );
};

render(React.createElement(App), document.getElementById('root'));
