import React from 'react';
import { SlateEditor, SlateToolbar, SlateContent } from 'slate-editor';
import {
  AlignmentPlugin,
  AlignmentButtonBar,
} from '@slate-editor/alignment-plugin';
import { BoldPlugin, BoldButton } from '@slate-editor/bold-plugin';
import {
  ColorPlugin,
  ColorButton,
  ColorStateModel,
} from '@slate-editor/color-plugin';
import { EmbedPlugin, EmbedButton } from '@slate-editor/embed-plugin';
import {
  FontFamilyPlugin,
  FontFamilyDropdown,
} from '@slate-editor/font-family-plugin';
import { FontSizePlugin, FontSizeInput } from '@slate-editor/font-size-plugin';
import { GridPlugin, GridButtonBar } from '@slate-editor/grid-plugin';
import { ImagePlugin, ImageButton } from '@slate-editor/image-plugin';
import { ItalicPlugin, ItalicButton } from '@slate-editor/italic-plugin';
import { LinkPlugin, LinkButton } from '@slate-editor/link-plugin';
import { ListPlugin, ListButtonBar } from '@slate-editor/list-plugin';
import {
  StrikethroughPlugin,
  StrikethroughButton,
} from '@slate-editor/strikethrough-plugin';
import {
  UnderlinePlugin,
  UnderlineButton,
} from '@slate-editor/underline-plugin';

const fontSizePluginOptions = { initialFontSize: 16 };
const colorPluginOptions = new ColorStateModel()
  .rgba({ r: 100, g: 100, b: 100, a: 1 })
  .gen();

const plugins = [
  AlignmentPlugin(),
  BoldPlugin(),
  ColorPlugin(),
  EmbedPlugin(),
  FontFamilyPlugin(),
  FontSizePlugin(fontSizePluginOptions),
  GridPlugin(),
  ImagePlugin(),
  ItalicPlugin(),
  LinkPlugin(),
  ListPlugin(),
  StrikethroughPlugin(),
  UnderlinePlugin(),
];

const Editor = () => {
return (
  <SlateEditor plugins={plugins}>
    <SlateToolbar>
      <BoldButton />
      <ItalicButton />
      <UnderlineButton />
      <StrikethroughButton />
      <AlignmentButtonBar />
      <LinkButton />
      <ListButtonBar />
    </SlateToolbar>

    <SlateToolbar>
      <FontFamilyDropdown />
      <FontSizeInput {...fontSizePluginOptions} />
      <ImageButton signingUrl={'http://localhost:888/sign'} />
      <ColorButton
        initialState={colorPluginOptions}
        pickerDefaultPosition={{ x: -520, y: 17 }}
      />
      <GridButtonBar />
      <EmbedButton />
    </SlateToolbar>

    <SlateContent />
  </SlateEditor>
);
};

export default Editor;
