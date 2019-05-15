import { USER_LOGIN, USER_LOGOUT } from '../actions/types';

const user = JSON.parse(localStorage.getItem('user'));
const initialState = user ? { user } : { user: null };

export default function(state = initialState, action) {
    switch (action.type) {
        case USER_LOGIN:
            localStorage.setItem('user', JSON.stringify(action.payload));
            return {
                ...state,
                user: action.payload,
            };
        case USER_LOGOUT:
            return {
                ...state,
                user: null,
            };
        default:
            return state;
    }
}
