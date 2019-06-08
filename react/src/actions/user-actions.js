import {
    USER_LOGIN_SUCCESS,
    USER_LOGIN,
    USER_LOGOUT,
    USER_LOGIN_FAILURE,
} from './types';
import * as alertActions from './alerts';

export const login = loginData => dispatch => {
    const { emailAddress } = loginData;
    dispatch({
        type: USER_LOGIN,
        payload: emailAddress,
    });
    window.api
        .postLogin(loginData)
        .then(res => res.data)
        .then(data =>
            dispatch({
                type: USER_LOGIN_SUCCESS,
                payload: data,
            }),
        )
        .then(dispatch(alertActions.success('Login successful')))
        .catch(error => {
            logout();
            dispatch({
                type: USER_LOGIN_FAILURE,
                payload: error,
            });
            dispatch(
                alertActions.error(
                    'You entered the wrong email address or password',
                ),
            );
        });
};

export const logout = () => dispatch => {
    window.api
        .getLogout()
        .then(res => res.data)
        .then(data =>
            dispatch({
                type: USER_LOGOUT,
                payload: data,
            }),
        );
};
