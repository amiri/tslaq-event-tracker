import { useEffect, useContext } from 'react';
import ReactGA from 'react-ga';
import { withRouter, useHistory } from 'react-router';
import { AuthContext } from '../contexts/AuthContext';
import { has } from 'lodash';

const sendPageView = ({ location, user }) => {
  if (location) {
    const options = {
      page: location.pathname,
      ...(user && { userId: user.authUserId }),
      ...(has(location, ['state', 'analytics', 'options']) && {
        ...location.state.analytics.options,
      }),
    };
    // console.log('sendPageView options: ', options);
    // console.log('sendPageView location: ', location);
    // console.log('sendPageView user: ', user);
    ReactGA.set(options);
    ReactGA.pageview(location.pathname);
  }
};

const Analytics = ({ children, trackingId }) => {
  const { user } = useContext(AuthContext);
  const history = useHistory();
  const { location } = history;
  useEffect(() => {
    sendPageView({ location: history.location, user });
    return history.listen(sendPageView);
  }, [history, location, trackingId]);
  return children;
};
Analytics.whyDidYouRender = true;

export default withRouter(Analytics);
