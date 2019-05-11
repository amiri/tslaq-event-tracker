import { combineReducers } from 'redux';
import pricesReducer from './prices-reducer';

export default combineReducers({
    prices: pricesReducer,
});
