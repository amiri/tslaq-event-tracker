import Api from '../api';
import { USER_LOGIN } from './types';

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
