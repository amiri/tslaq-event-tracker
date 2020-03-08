import React, { useState, useRef } from 'react';
import { createEditor } from 'slate';
import { Slate, Editable, withReact } from 'slate-react';
import { withHistory } from 'slate-history';
import Toolbar from './Qeditor/Toolbar';
import { withImages, insertImage } from './Qeditor/Image';
import { withTwitter } from './Qeditor/Twitter';
import { withLinks } from './Qeditor/Link';
import { renderLeaf, renderElement } from './Qeditor/Render';

const Qeditor = ({ body, onChange, onBlur, eventId }) => {
  const editor = withTwitter(
    withImages(withLinks(withHistory(withReact(createEditor())))),
  );
  const editorRef = useRef(editor);
  const [value, setValue] = useState(body);

  return (
    <Slate
      editor={editorRef.current}
      value={value}
      onChange={change => {
        const storedImageUploads = sessionStorage.getItem('imageUploads');
        const location = sessionStorage.getItem('imageInsertLocation');
        const imageInsertLocation = location ? JSON.parse(location) : null;
          console.log('imageInsertLocation: ', imageInsertLocation);
        const imageUploads = storedImageUploads
          ? JSON.parse(storedImageUploads)
          : [];
        if (imageUploads.length) {
          imageUploads.map(i => insertImage(editorRef.current, i, location));
        }
        const newValue = imageUploads.length
          ? editorRef.current.children
          : change;
        sessionStorage.removeItem('imageUploads');
        // sessionStorage.removeItem('imageInsertLocation');
        setValue(newValue);
        onChange(newValue);
      }}
      onBlur={onBlur}
    >
      <Toolbar eventId={eventId} />
      <Editable renderElement={renderElement} renderLeaf={renderLeaf} />
    </Slate>
  );
};
Qeditor.whyDidYouRender = true;

export default Qeditor;
