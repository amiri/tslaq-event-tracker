import {
    USER_LOGIN_SUCCESS,
    USER_LOGIN,
    USER_LOGOUT,
    USER_LOGIN_FAILURE,
} from '../actions/types';

const user = JSON.parse(localStorage.getItem('user'));
const initialState = user ? { user } : { user: null };

export default function(state = initialState, action) {
    switch (action.type) {
        case USER_LOGIN:
            return {
                ...state,
                loggingIn: true,
            };
        case USER_LOGIN_SUCCESS:
            localStorage.setItem('user', JSON.stringify(action.payload));
            return {
                ...state,
                user: action.payload,
                loggingIn: false,
            };
        case USER_LOGOUT:
            return {
                ...state,
                user: null,
                loggingIn: false,
            };
        case USER_LOGIN_FAILURE:
            return {
                ...state,
                user: null,
                loggingIn: false,
            };
        default:
            return state;
    }
}
