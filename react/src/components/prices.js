import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { getPrices } from '../actions/price-actions';

class Prices extends Component {
    componentWillMount() {
        this.props.getPrices();
    }

    render() {
        return <div />;
    }
}

Prices.propTypes = {
    getPrices: PropTypes.func.isRequired,
    prices: PropTypes.array.isRequired,
};

const mapStateToProps = state => ({
    prices: state.prices.items,
});

export default connect(
    mapStateToProps,
    { getPrices },
)(Prices);
