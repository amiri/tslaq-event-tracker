import Api from '../api';
import { USER_LOGIN, USER_LOGOUT } from './types';

export const login = loginData => dispatch => {
    window.api = new Api();
    window.api
        .postLogin(loginData)
        .then(res => res.data)
        .then(data =>
            dispatch({
                type: USER_LOGIN,
                payload: data,
            }),
        );
};

export const logout = user => dispatch => {
    window.api = new Api();
    window.api
        .getLogout()
        .then(res => res.data)
        .then(data =>
            dispatch({
                type: USER_LOGOUT,
                payload: user,
            }),
        );
};
