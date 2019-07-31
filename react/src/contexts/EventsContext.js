import React, { createContext, useState, useEffect } from 'react';

export const EventsContext = createContext();

const EventsContextProvider = props => {
  const [events, setEvents] = useState([]);

  async function getEvents() {
    const e = await window.api.getEvents().then(res => res.data);
    setEvents(e);
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
    <EventsContext.Provider value={{ events, setEvents }}>
      {props.children}
    </EventsContext.Provider>
  );
};

export default EventsContextProvider;
