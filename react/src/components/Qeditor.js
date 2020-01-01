import React, { useState, useMemo } from 'react';
import { createEditor } from 'slate';
import { Slate, Editable, withReact } from 'slate-react';
import { withHistory } from 'slate-history';
import Toolbar from './Qeditor/Toolbar';
import { withImages } from './Qeditor/Image';
import { withTwitter } from './Qeditor/Twitter';
import { withLinks } from './Qeditor/Link';
import { renderLeaf, renderElement } from './Qeditor/Render';

const Qeditor = ({ body, onChange, onBlur }) => {
  const editor = useMemo(
    () =>
      withTwitter(
        withImages(withLinks(withHistory(withReact(createEditor())))),
      ),
    [],
  );
  const [value, setValue] = useState(body);

  return (
    <Slate
      editor={editor}
      value={value}
      onChange={change => {
        onChange(change);
        setValue(change);
      }}
      onBlur={onBlur}
    >
      <Toolbar />
      <Editable renderElement={renderElement} renderLeaf={renderLeaf} />
    </Slate>
  );
};

export default Qeditor;
