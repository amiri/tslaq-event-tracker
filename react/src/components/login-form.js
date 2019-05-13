import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { login } from '../actions/user-actions';

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
        this.props.login(loginData);
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

LoginForm.propTypes = {
    login: PropTypes.func.isRequired,
    user: PropTypes.object,
};

const mapStateToProps = state => ({
    login: state.login,
    user: state.user.user,
});

const mapDispatchToProps = dispatch => {
    return {
        login: loginData => dispatch(login(loginData)),
    };
};

export default connect(
    mapStateToProps,
    mapDispatchToProps,
)(LoginForm);
