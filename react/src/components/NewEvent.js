import React, { useEffect, useContext } from 'react';
import { ModalContext } from '../contexts/ModalContext';
import { ChartContext } from '../contexts/ChartContext';
import EventForm from './EventForm';
import { Modal, Select } from 'antd';
import {
  isArray,
  isObject,
  mapKeys,
  mapValues,
  isNil,
  get,
  has,
  set,
} from 'lodash';

const { Option, OptGroup } = Select;

const renameKeys = (names, obj) => {
  if (isArray(obj)) {
    return obj.map(inner => renameKeys(names, inner));
  } else if (isObject(obj)) {
    const res = mapKeys(obj, (v, k) => names[k] || k);
    return mapValues(res, v => renameKeys(names, v));
  } else {
    return obj;
  }
};

const transformOpt = (obj) => {
    return Object.keys(obj).reduce((os, k) => {
        if (k === 'direct') {
            os.push(obj[k].sort((a,b) => (a.fullName > b.fullName) ? 1 : -1).map(o => (<Option key={o.id} value={o.id} label={o.name}>{o.fullName}</Option>)));
        } else {
            os.push(<OptGroup label={k} key={k}>{transformOpt(obj[k])}</OptGroup>);
        }
        return os;
    }, []);
};

const NewEvent = props => {
  const { visible, setVisible } = useContext(ModalContext);
  const { allCategories } = useContext(ChartContext);
  let ns = {};
  const opts = allCategories.reduce((os, c) => {
    ns[c.id] = c.name;
    if (isNil(c.parents)) {
      const there = get(os, c.id, { direct: [] });
      there.direct.push(c);
      set(os, c.id, there);
    } else {
      const there = get(os, c.parents, { direct: [] });
      if (has(there, c.id)) {
        there[c.id].direct.push(c);
      } else {
        there.direct.push(c);
      }
      set(os, c.parents, there);
    }
    return os;
  }, {});
  const renamed = renameKeys(ns, opts);
  const treeOpts = Object.keys(renamed).reduce((os, k) => {
    os.push(<OptGroup label={k} key={k}>{transformOpt(renamed[k])}</OptGroup>);
    return os;
  }, []);
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
        categoryOptions={treeOpts}
        event={{ time: eventDate }}
        setVisible={setVisible}
      />
    </Modal>
  );
};

export default NewEvent;
