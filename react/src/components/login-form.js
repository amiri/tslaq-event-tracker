import React, { Component } from 'react';

class LoginForm extends Component {
    constructor(props) {
        super(props);
        this.state = {
            email: '',
            password: '',
        };
        this.onChange = this.onChange.bind(this);
        this.onSubmit = this.onSubmit.bind(this);
    }

    onChange(e) {
        this.setState({
            [e.target.name]: e.target.value,
        });
    }

    onSubmit(e) {
        e.preventDefault();
        const loginData = {
            emailAddress: this.state.email,
            password: this.state.password,
        };
        window.api
            .postLogin(JSON.stringify(loginData))
            .then(res => console.log(res));
    }

    render() {
        return (
            <div className="Login">
                <form onSubmit={this.onSubmit}>
                    <div>
                        <label>Email</label>
                        <input
                            type="text"
                            name="email"
                            onChange={this.onChange}
                            value={this.state.emailAddress}
                        />
                        <label>Password</label>
                        <input
                            type="password"
                            name="password"
                            onChange={this.onChange}
                            value={this.state.password}
                        />
                        <button type="submit">Submit</button>
                    </div>
                </form>
            </div>
        );
    }
}

export default LoginForm;
