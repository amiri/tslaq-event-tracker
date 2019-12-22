export const authReducer = (state, action) => {
  console.log(state);
  switch (action.type) {
    case 'LOGIN_SUCCESS': {
      sessionStorage.setItem('user', JSON.stringify(action.payload));
      // eslint-disable-next-line no-unused-vars
      const { apiError, ...noApiError } = state;
      return {
        ...noApiError,
        user: action.payload,
      };
    }
    case 'LOGIN_FAILURE': {
      return {
        ...state,
        apiError: action.payload,
      };
    }
    case 'LOGOUT': {
      // eslint-disable-next-line no-unused-vars
      const { user, apiError, ...noUserOrApiError } = state;
      return {
        ...noUserOrApiError,
      };
    }
    case 'REGISTER_SUCCESS': {
      sessionStorage.setItem('user', JSON.stringify(action.payload));
      return {
        ...state,
        user: action.payload,
      };
    }
    case 'REGISTER_FAILURE': {
      return {
        ...state,
        apiError: action.payload,
      };
    }
    default: {
      return state;
    }
  }
};
