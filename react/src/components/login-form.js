import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { Alert, Form, Icon, Input, Button, Spin } from 'antd';
import { login, logout } from '../actions/user-actions';

function hasErrors(fieldsError) {
    return Object.keys(fieldsError).some(field => fieldsError[field]);
}

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

    componentDidMount() {
        this.props.form.validateFields();
    }

    onChange(e) {
        this.setState({
            [e.target.name]: e.target.value,
        });
    }

    onSubmit(e) {
        e.preventDefault();
        this.props.form.validateFields((err, values) => {
            if (!err) {
                console.log('Received values of form:', values);
                const loginData = {
                    emailAddress: values.email,
                    password: values.password,
                };
                this.props.login(loginData);
            }
        });
    }

    handleLogout(e) {
        e.preventDefault();
        localStorage.removeItem('user');
        this.props.logout(this.props.user);
    }

    render() {
        const authUser = this.props.user;
        const alert = this.props.alert;
        const { loggingIn } = this.props;

        const {
            getFieldDecorator,
            getFieldsError,
            getFieldError,
            isFieldTouched,
        } = this.props.form;

        // Only show error after a field is touched.
        const emailError = isFieldTouched('email') && getFieldError('email');
        const passwordError =
            isFieldTouched('password') && getFieldError('password');

        return authUser ? (
            <div className="Logout">
                {alert.message && (
                    <Alert
                        message={alert.message}
                        type={alert.type}
                        duration={1}
                    />
                )}

                <form onSubmit={this.handleLogout}>
                    <button type="submit">Logout</button>
                </form>
            </div>
        ) : loggingIn ? (
            <Spin />
        ) : (
            <div className="Login">
                <Form layout="inline" onSubmit={this.onSubmit}>
                    {alert.message && (
                        <Alert
                            message={alert.message}
                            type={alert.type}
                            duration={1}
                        />
                    )}
                    <Form.Item
                        validateStatus={emailError ? 'error' : ''}
                        help={emailError || ''}
                    >
                        {getFieldDecorator('email', {
                            rules: [
                                {
                                    required: true,
                                    message: 'Please enter your email address.',
                                },
                            ],
                        })(
                            <Input
                                prefix={
                                    <Icon
                                        type="email"
                                        style={{ color: 'rgba(0,0,0,.25)' }}
                                    />
                                }
                                placeholder="me@me.com"
                            />,
                        )}
                    </Form.Item>
                    <Form.Item
                        validateStatus={passwordError ? 'error' : ''}
                        help={passwordError || ''}
                    >
                        {getFieldDecorator('password', {
                            rules: [
                                {
                                    required: true,
                                    message: 'Please enter your password.',
                                },
                            ],
                        })(
                            <Input
                                prefix={
                                    <Icon
                                        type="lock"
                                        style={{ color: 'rgba(0,0,0,.25)' }}
                                    />
                                }
                                type="password"
                                placeholder="abc123"
                            />,
                        )}
                    </Form.Item>
                    <Form.Item>
                        <Button
                            type="primary"
                            htmlType="submit"
                            disabled={hasErrors(getFieldsError())}
                        >
                            Log in
                        </Button>
                    </Form.Item>
                </Form>
            </div>
            //    <div className="Login">
            //        <form onSubmit={this.onSubmit}>
            //            <div>
            //                <label>Email</label>
            //                <input
            //                    type="text"
            //                    name="email"
            //                    onChange={this.onChange}
            //                    value={this.state.emailAddress}
            //                />
            //                <label>Password</label>
            //                <input
            //                    type="password"
            //                    name="password"
            //                    onChange={this.onChange}
            //                    value={this.state.password}
            //                />
            //                <button type="submit">Submit</button>
            //            </div>
            //        </form>
            //    </div>
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
    loggingIn: state.user.loggingIn,
    alert: state.alerts,
});

const mapDispatchToProps = dispatch => {
    return {
        login: loginData => dispatch(login(loginData)),
        logout: user => dispatch(logout(user)),
    };
};

const AntLoginForm = Form.create({ name: 'login' })(LoginForm);
export default connect(
    mapStateToProps,
    mapDispatchToProps,
)(AntLoginForm);
