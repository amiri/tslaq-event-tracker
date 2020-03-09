import React from 'react';
import { render } from 'react-dom';
import Api from './Api';
import PricesContextProvider from './contexts/PricesContext';
import EventsContextProvider from './contexts/EventsContext';
import AuthContextProvider from './contexts/AuthContext';
import ChartContextProvider from './contexts/ChartContext';
import NewEventModalContextProvider from './contexts/NewEventModalContext';
import EditEventModalContextProvider from './contexts/EditEventModalContext';
import NewCategoryModalContextProvider from './contexts/NewCategoryModalContext';
import ImportModalContextProvider from './contexts/ImportModalContext';
import ImageModalContextProvider from './contexts/ImageModalContext';
import AuthModalContextProvider from './contexts/AuthModalContext';
import NavBar from './components/NavBar';
import Chart from './components/Chart';
import NewEvent from './components/NewEvent';
import EditEvent from './components/EditEvent';
import Import from './components/Import';
import NewCategory from './components/NewCategory';
import ImageUpload from './components/ImageUpload';
import Auth, { UserRequired } from './components/Auth';
import './App.css';
import { Layout } from 'antd';
import { BrowserRouter as Router, Route } from 'react-router-dom';
import Analytics from './components/Analytics';
import ReactGA from 'react-ga';

const whyDidYouRender = require('@welldone-software/why-did-you-render');
whyDidYouRender(React, {
  trackAllPureComponents: true,
  // logOnDifferentValues: true,
  include: [/A-za-z/],
});

const { Header, Content } = Layout;
window.api = new Api();

const trackingId = 'UA-158948307-1';
ReactGA.initialize(trackingId);

const handleObservations = list => {
  list.getEntries().forEach(entry => {
    // console.log('handleObservations typeof entry: ', typeof entry);
    // console.log('handleObservations entry: ', entry);
    const timed = {
      category: entry.entryType,
      variable: entry.name,
      value:
        entry.entryType === 'resource' ? entry.duration : entry.domInteractive,
    };
    // console.log('handleObservations timed: ', timed);
    ReactGA.timing(timed);
  });
};

const observer = new PerformanceObserver(handleObservations);
observer.observe({ entryTypes: ['navigation', 'resource'] });

const App = () => {
  return (
    // <React.StrictMode>
    <AuthContextProvider>
      <PricesContextProvider>
        <EventsContextProvider>
          <ChartContextProvider>
            <NewEventModalContextProvider>
              <EditEventModalContextProvider>
                <NewCategoryModalContextProvider>
                  <ImportModalContextProvider>
                    <ImageModalContextProvider>
                      <AuthModalContextProvider>
                        <Router>
                          <Analytics trackingId={trackingId}>
                            <Layout style={{ height: '100%', width: '100%' }}>
                              <Header style={{ backgroundColor: '#f0f2f5' }}>
                                <Route
                                  path='/'
                                  render={props => <NavBar {...props} />}
                                />
                              </Header>
                              <Content
                                style={{ height: '100%', width: '100%' }}
                              >
                                <Route
                                  path='/'
                                  render={props => <Chart {...props} />}
                                />
                                <UserRequired
                                  path='/new'
                                  render={props => <NewEvent {...props} />}
                                />
                                <UserRequired
                                  path='/new/category'
                                  render={props => <NewCategory {...props} />}
                                />
                                <Route
                                  path='/login'
                                  render={props => <Auth {...props} />}
                                />
                                <Route
                                  path='/register'
                                  render={props => <Auth {...props} />}
                                />
                                <UserRequired
                                  path='/category'
                                  render={props => <NewCategory {...props} />}
                                />
                                <UserRequired
                                  path='/event/edit'
                                  render={props => <EditEvent {...props} />}
                                />
                                <UserRequired
                                  path='/event/edit/image'
                                  render={props => <ImageUpload {...props} />}
                                />
                                <UserRequired
                                  path='/import'
                                  render={props => <Import {...props} />}
                                />
                              </Content>
                            </Layout>
                          </Analytics>
                        </Router>
                      </AuthModalContextProvider>
                    </ImageModalContextProvider>
                  </ImportModalContextProvider>
                </NewCategoryModalContextProvider>
              </EditEventModalContextProvider>
            </NewEventModalContextProvider>
          </ChartContextProvider>
        </EventsContextProvider>
      </PricesContextProvider>
    </AuthContextProvider>
    // </React.StrictMode>
  );
};

//App.whyDidYouRender = true;

render(React.createElement(App), document.getElementById('root'));
