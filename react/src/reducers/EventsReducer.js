export const eventsReducer = (state, action) => {
  switch (action.type) {
    case 'GET_EVENTS': {
      return action.payload;
    }
    case 'POST_EVENT': {
      return [...state, action.payload];
    }
    case 'UPDATE_EVENT': {
      const updatedId = action.payload.id;
      const newState = state.filter(e => e.id !== updatedId);
      return [...newState, action.payload];
    }
    default: {
      return state;
    }
  }
};
