import React, { useMemo, useEffect, useContext } from 'react';
import { ModalContext } from '../contexts/ModalContext';
import { includes, isNil, isEmpty } from 'lodash';
import * as QueryString from 'query-string';
import { Modal, Divider, Typography } from 'antd';
import { decryptIds } from './utils/Chart';
import { createEditor } from 'slate';
import { Slate, Editable, withReact } from 'slate-react';
import { renderLeaf, renderElement } from './Qeditor/Render';

const { Text } = Typography;
const EventsDetail = props => {
  const { visible, setVisible } = useContext(ModalContext);
  const { history, location, events = [] } = props;
  const params = QueryString.parse(location.search);
  const eventIds = !isNil(params.id) ? decryptIds({ ids: params.id }) : [];
  useEffect(() => {
    setVisible(location.state.visible);
  }, [location]);
  const handleClose = () => {
    setVisible(false);
    history.goBack();
  };
  const eventsToDisplay = !isEmpty(eventIds)
    ? events.filter(e => includes(eventIds, e.id))
    : events;

  const editor = useMemo(() => withReact(createEditor()), []);

  const eventDisplays = eventsToDisplay.map(e => {
    return (
      <div key={e.id}>
        <Text strong>{e.title}</Text>
        <br />
        <Text>{e.time}</Text>
        <br />
        <p className='byline'>
          <em>
            {e.author}, {e.createTime}
          </em>
        </p>
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
      title='Events'
      visible={visible}
      onOk={handleClose}
      onCancel={handleClose}
    >
      {eventDisplays}
    </Modal>
  );
};

export default EventsDetail;
