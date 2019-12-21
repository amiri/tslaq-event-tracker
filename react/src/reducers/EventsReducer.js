export const eventsReducer = (state, action) => {
  switch (action.type) {
    case 'GET_EVENTS': {
      return action.payload;
    }
    case 'POST_EVENTS': {
      console.log(state);
        console.log(action.payload);
    }
    default: {
      return state;
    }
  }
};
