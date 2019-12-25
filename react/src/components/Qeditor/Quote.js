import { Editor, Transforms } from 'slate';

export const isQuote = editor => {
  const [match] = Editor.nodes(editor, {
    match: n => n.type === 'quote',
  });
  return !!match;
};

export const toggleQuote = editor => {
  const isActive = isQuote(editor);
  Transforms.setNodes(
    editor,
    { type: isActive ? null : 'quote' },
    { match: n => Editor.isBlock(editor, n) },
  );
};
