import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { login, logout } from '../actions/user-actions';

class LoginForm extends Component {
    constructor(props) {
        super(props);
        this.state = {
            email: '',
            password: '',
        };
        this.onChange = this.onChange.bind(this);
        this.onSubmit = this.onSubmit.bind(this);
        this.handleLogout = this.handleLogout.bind(this);
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
    handleLogout(e) {
        e.preventDefault();
        localStorage.removeItem('user');
        this.props.logout(this.props.user);
    }

    render() {
        const authUser = this.props.user;
        return authUser ? (
            <div className="Logout">
                <form onSubmit={this.handleLogout}>
                    <button type="submit">Logout</button>
                </form>
            </div>
        ) : (
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
    logout: PropTypes.func.isRequired,
    user: PropTypes.object,
};

const mapStateToProps = state => ({
    login: state.login,
    logout: state.logout,
    user: state.user.user,
});

const mapDispatchToProps = dispatch => {
    return {
        login: loginData => dispatch(login(loginData)),
        logout: user => dispatch(logout(user)),
    };
};

export default connect(
    mapStateToProps,
    mapDispatchToProps,
)(LoginForm);
