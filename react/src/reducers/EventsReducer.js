export const eventsReducer = (state, action) => {
  console.log('eventsReducer state: ', state);
  console.log('eventsReducer action: ', action);
  switch (action.type) {
    case 'GET_EVENTS': {
      return action.payload;
    }
    case 'POST_EVENT': {
      return [...state, action.payload];
    }
    default: {
      return state;
    }
  }
};
