/* eslint-disable no-unused-vars */
import React, { Component } from 'react';
import './app.css';
import { Provider } from 'react-redux';
import Api from './api';
import Chart from './components/chart';
import LoginForm from './components/login-form';
import store from './store';

class App extends Component {
    render() {
        return (
            <Provider store={store}>
                <div className="App">
                    <header className="App-header" />
                    <LoginForm />
                    <div className="Chart">
                        <Chart />
                    </div>
                </div>
            </Provider>
        );
    }
}

export default App;
