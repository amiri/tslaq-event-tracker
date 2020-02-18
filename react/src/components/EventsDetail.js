import React, { useMemo, useEffect, useContext } from 'react';
import { NewEventModalContext } from '../contexts/NewEventModalContext';
import { includes, isNil, isEmpty } from 'lodash';
import * as QueryString from 'query-string';
import { Tag, Modal, Divider, Typography } from 'antd';
import { decryptIds } from './utils/Chart';
import { createEditor } from 'slate';
import { Slate, Editable, withReact } from 'slate-react';
import { renderLeaf, renderElement } from './Qeditor/Render';
import moment from 'moment';
require('moment-timezone');

const { Title, Text } = Typography;
const EventsDetail = props => {
  const { visible, setVisible } = useContext(NewEventModalContext);
  const { history, location, events = [], colorScale } = props;
  const params = QueryString.parse(location.search);
  const eventIds = !isNil(params.id) ? decryptIds({ ids: params.id }) : [];
  useEffect(() => {
    setVisible(location.state.visible);
  }, [location]);

  const handleClose = () => {
    setVisible(false);
    history.push('/');
  };

  const eventsToDisplay = !isEmpty(eventIds)
    ? events.filter(e => includes(eventIds, e.id))
    : events;

  const editor = useMemo(() => withReact(createEditor()), []);

  const eventDisplays = eventsToDisplay.map(e => {
    return (
      <div key={e.id}>
        <Title level={3}>{e.title}</Title>
        <br />
        <Text strong>
          {moment
            .utc(e.time)
            .tz('America/New_York')
            .format('dddd, MMMM D, YYYY, h:mm:ss A zz')}
        </Text>
        <br />
        <p className='byline'>
          <em>
            {e.author} at{' '}
            {moment
              .utc(e.createTime)
              .tz('America/New_York')
              .format('dddd, MMMM D, YYYY, h:mm:ss A zz')}
          </em>
        </p>
        <div className='tag-list'>
          {!isNil(e.categories)
            ? e.categories.map(c => (
                <Tag key={c.id} color={colorScale(c.fullName)}>
                  {c.fullName}
                </Tag>
              ))
            : null}
        </div>
        <br />
        <Slate editor={editor} value={JSON.parse(e.body)}>
          <Editable
            readOnly
            renderElement={renderElement}
            renderLeaf={renderLeaf}
          />
        </Slate>
        <Divider />
      </div>
    );
  });

  return (
    <Modal
      title={<Title level={2}>Events</Title>}
      visible={visible}
      onOk={handleClose}
      onCancel={handleClose}
      footer={false}
    >
      {eventDisplays}
    </Modal>
  );
};

export default EventsDetail;
