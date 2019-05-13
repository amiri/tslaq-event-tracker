import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { getPrices, getEvents } from '../actions/chart-actions';

class Chart extends Component {
    componentWillMount() {
        this.props.getPrices();
        this.props.getEvents();
    }

    render() {
        return <div />;
    }
}

Chart.propTypes = {
    getPrices: PropTypes.func.isRequired,
    getEvents: PropTypes.func.isRequired,
    prices: PropTypes.array.isRequired,
    events: PropTypes.array.isRequired,
};

const mapStateToProps = state => ({
    prices: state.prices.prices,
    events: state.events.events,
});

const mapDispatchToProps = {
    getPrices,
    getEvents,
};

export default connect(
    mapStateToProps,
    mapDispatchToProps,
)(Chart);
