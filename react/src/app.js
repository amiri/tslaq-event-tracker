/* eslint-disable no-unused-vars */
import React, { Component } from 'react';
import './app.css';
import { Provider } from 'react-redux';
import Api from './api';
import Prices from './components/prices';
import LoginForm from './components/login-form';
import store from './store';

class App extends Component {
    render() {
        return (
            <Provider store={store}>
                <div className="App">
                    <header className="App-header" />
                    <LoginForm />
                    <div className="Prices">
                        <Prices />
                    </div>
                </div>
            </Provider>
        );
    }
}

export default App;
