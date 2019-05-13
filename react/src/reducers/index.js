import { combineReducers } from 'redux';
import prices from './prices';
import events from './events';
import user from './authentication';

export default combineReducers({
    prices,
    events,
    user,
});
