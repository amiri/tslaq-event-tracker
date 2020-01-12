import React, { useEffect, useContext } from 'react';
import { ModalContext } from '../contexts/ModalContext';
import { ChartContext } from '../contexts/ChartContext';
import EventForm from './EventForm';
import { Modal, Select } from 'antd';
import {sortBy, isNil, get, has, set, compact} from 'lodash';

const { Option, OptGroup } = Select;

const NewEvent = props => {
  const { visible, setVisible } = useContext(ModalContext);
  const { allCategories } = useContext(ChartContext);
  let ns = {};
  console.log(allCategories);
  const opts = allCategories.reduce((os, c) => {
    ns[c.id] = c.name;
    if (isNil(c.parents)) {
        const there = get(os, c.id, []);
        there.push(c);
        set(os, c.id, there);
    } else {
        const there = get(os, c.parents, []);
        there.push(c);
        set(os, c.parents, there);
    }
    return os;
  }, {});
  console.log(opts);
    console.log(ns);
  const categoryOptions = allCategories.map(o => (
    <Option key={o.id} value={o.id} label={o.name}>
      {o.name}
    </Option>
  ));
  const { history, location } = props;
  const eventDate = location.state.eventDate;

  useEffect(() => {
    setVisible(location.state.visible);
  }, [location]);

  const handleClose = () => {
    setVisible(false);
    history.push('/');
  };

  return (
    <Modal
      title='New Event'
      destroyOnClose={true}
      visible={visible}
      onCancel={handleClose}
      footer={false}
    >
      <EventForm
        categoryOptions={categoryOptions}
        event={{ time: eventDate }}
        setVisible={setVisible}
      />
    </Modal>
  );
};

export default NewEvent;
