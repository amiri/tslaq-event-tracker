import { combineReducers } from 'redux';
import prices from './prices';
import events from './events';
import user from './authentication';
import alerts from './alerts';

export default combineReducers({
    prices,
    events,
    user,
    alerts,
});
