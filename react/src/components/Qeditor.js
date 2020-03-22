import React, { useState, useRef } from 'react';
import { createEditor } from 'slate';
import { Slate, Editable, withReact } from 'slate-react';
import { withHistory } from 'slate-history';
import Toolbar from './Qeditor/Toolbar';
import { withImages, insertImage } from './Qeditor/Image';
import { withTwitter } from './Qeditor/Twitter';
import { withLinks } from './Qeditor/Link';
import { renderLeaf, renderElement } from './Qeditor/Render';

const getUpdatedBody = ({ body, editorRef }) => {
  const storedImageUploads = sessionStorage.getItem('imageUploads');
  const imageLocation = sessionStorage.getItem('imageInsertLocation');
  const imageInsertLocation = imageLocation ? JSON.parse(imageLocation) : null;
  const imageUploads = storedImageUploads ? JSON.parse(storedImageUploads) : [];
  if (imageUploads.length) {
    imageUploads.map(i =>
      insertImage(editorRef.current, i, imageInsertLocation),
    );
  }
  const newValue = imageUploads.length ? editorRef.current.children : body;
  sessionStorage.removeItem('imageUploads');
  return newValue;
};

const Qeditor = ({ body, formikChange, onBlur, eventId }) => {
  const editor = withTwitter(
    withImages(withLinks(withHistory(withReact(createEditor())))),
  );

  const editorRef = useRef(editor);

  const updatedBody = getUpdatedBody({ body, editorRef });
  const [value, setValue] = useState(updatedBody);

  return (
    <Slate
      editor={editorRef.current}
      value={value}
      onChange={change => {
        setValue(change);
        formikChange(change);
      }}
      onBlur={onBlur}
    >
      <Toolbar eventId={eventId} />
      <Editable renderElement={renderElement} renderLeaf={renderLeaf} />
    </Slate>
  );
};
// Qeditor.whyDidYouRender = true;

export default Qeditor;
