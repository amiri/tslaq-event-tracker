import { GET_PRICES } from '../actions/types';

const initialState = {
    prices: [],
};

export default function(state = initialState, action) {
    switch (action.type) {
        case GET_PRICES:
            return {
                ...state,
                items: action.payload,
            };
        default:
            return state;
    }
}
