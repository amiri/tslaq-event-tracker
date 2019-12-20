import React, { createContext, useEffect, useReducer, useState } from 'react';
import { eventsReducer } from '../reducers/EventsReducer';

export const EventsContext = createContext();

const s = [];

const EventsContextProvider = props => {
  const [events, dispatch] = useReducer(eventsReducer, s);
  const [filteredEvents, setFilteredEvents] = useState([]);

  async function getEvents() {
    await window.api.getEvents().then(res =>
      dispatch({
        type: 'GET_EVENTS',
        payload: res.data,
      }),
    );
  }

  useEffect(() => {
    getEvents();
  }, []);

  // Reload events every hour
  useEffect(() => {
    const timer = setTimeout(() => {
      getEvents();
    }, 3600000);
    return () => clearTimeout(timer);
  });

  return (
    <EventsContext.Provider
      value={{ events, dispatch, filteredEvents, setFilteredEvents }}
    >
      {props.children}
    </EventsContext.Provider>
  );
};

export default EventsContextProvider;
