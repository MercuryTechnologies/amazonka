{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectParticipant.Types.Item
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types.Item where

import Amazonka.ConnectParticipant.Types.AttachmentItem
import Amazonka.ConnectParticipant.Types.ChatItemType
import Amazonka.ConnectParticipant.Types.ParticipantRole
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An item - message or event - that has been sent.
--
-- /See:/ 'newItem' smart constructor.
data Item = Item'
  { -- | The ID of the sender in the session.
    participantId :: Prelude.Maybe Prelude.Text,
    -- | The time when the message or event was sent.
    --
    -- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
    -- example, 2019-11-08T02:41:28.172Z.
    absoluteTime :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the attachments.
    attachments :: Prelude.Maybe [AttachmentItem],
    -- | The role of the sender. For example, is it a customer, agent, or system.
    participantRole :: Prelude.Maybe ParticipantRole,
    -- | The content of the message or event.
    content :: Prelude.Maybe Prelude.Text,
    -- | The ID of the item.
    id :: Prelude.Maybe Prelude.Text,
    -- | The chat display name of the sender.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Type of the item: message or event.
    type' :: Prelude.Maybe ChatItemType,
    -- | The type of content of the item.
    contentType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Item' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participantId', 'item_participantId' - The ID of the sender in the session.
--
-- 'absoluteTime', 'item_absoluteTime' - The time when the message or event was sent.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
--
-- 'attachments', 'item_attachments' - Provides information about the attachments.
--
-- 'participantRole', 'item_participantRole' - The role of the sender. For example, is it a customer, agent, or system.
--
-- 'content', 'item_content' - The content of the message or event.
--
-- 'id', 'item_id' - The ID of the item.
--
-- 'displayName', 'item_displayName' - The chat display name of the sender.
--
-- 'type'', 'item_type' - Type of the item: message or event.
--
-- 'contentType', 'item_contentType' - The type of content of the item.
newItem ::
  Item
newItem =
  Item'
    { participantId = Prelude.Nothing,
      absoluteTime = Prelude.Nothing,
      attachments = Prelude.Nothing,
      participantRole = Prelude.Nothing,
      content = Prelude.Nothing,
      id = Prelude.Nothing,
      displayName = Prelude.Nothing,
      type' = Prelude.Nothing,
      contentType = Prelude.Nothing
    }

-- | The ID of the sender in the session.
item_participantId :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_participantId = Lens.lens (\Item' {participantId} -> participantId) (\s@Item' {} a -> s {participantId = a} :: Item)

-- | The time when the message or event was sent.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
item_absoluteTime :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_absoluteTime = Lens.lens (\Item' {absoluteTime} -> absoluteTime) (\s@Item' {} a -> s {absoluteTime = a} :: Item)

-- | Provides information about the attachments.
item_attachments :: Lens.Lens' Item (Prelude.Maybe [AttachmentItem])
item_attachments = Lens.lens (\Item' {attachments} -> attachments) (\s@Item' {} a -> s {attachments = a} :: Item) Prelude.. Lens.mapping Lens.coerced

-- | The role of the sender. For example, is it a customer, agent, or system.
item_participantRole :: Lens.Lens' Item (Prelude.Maybe ParticipantRole)
item_participantRole = Lens.lens (\Item' {participantRole} -> participantRole) (\s@Item' {} a -> s {participantRole = a} :: Item)

-- | The content of the message or event.
item_content :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_content = Lens.lens (\Item' {content} -> content) (\s@Item' {} a -> s {content = a} :: Item)

-- | The ID of the item.
item_id :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_id = Lens.lens (\Item' {id} -> id) (\s@Item' {} a -> s {id = a} :: Item)

-- | The chat display name of the sender.
item_displayName :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_displayName = Lens.lens (\Item' {displayName} -> displayName) (\s@Item' {} a -> s {displayName = a} :: Item)

-- | Type of the item: message or event.
item_type :: Lens.Lens' Item (Prelude.Maybe ChatItemType)
item_type = Lens.lens (\Item' {type'} -> type') (\s@Item' {} a -> s {type' = a} :: Item)

-- | The type of content of the item.
item_contentType :: Lens.Lens' Item (Prelude.Maybe Prelude.Text)
item_contentType = Lens.lens (\Item' {contentType} -> contentType) (\s@Item' {} a -> s {contentType = a} :: Item)

instance Core.FromJSON Item where
  parseJSON =
    Core.withObject
      "Item"
      ( \x ->
          Item'
            Prelude.<$> (x Core..:? "ParticipantId")
            Prelude.<*> (x Core..:? "AbsoluteTime")
            Prelude.<*> (x Core..:? "Attachments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ParticipantRole")
            Prelude.<*> (x Core..:? "Content")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "ContentType")
      )

instance Prelude.Hashable Item where
  hashWithSalt salt' Item' {..} =
    salt' `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` participantRole
      `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` absoluteTime
      `Prelude.hashWithSalt` participantId

instance Prelude.NFData Item where
  rnf Item' {..} =
    Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf participantRole
      `Prelude.seq` Prelude.rnf attachments
      `Prelude.seq` Prelude.rnf absoluteTime
