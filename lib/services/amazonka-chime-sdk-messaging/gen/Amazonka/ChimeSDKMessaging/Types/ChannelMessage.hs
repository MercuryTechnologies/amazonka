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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelMessage where

import Amazonka.ChimeSDKMessaging.Types.ChannelMessagePersistenceType
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatusStructure
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageType
import Amazonka.ChimeSDKMessaging.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of a message in a channel.
--
-- /See:/ 'newChannelMessage' smart constructor.
data ChannelMessage = ChannelMessage'
  { -- | The status of the channel message.
    status :: Prelude.Maybe ChannelMessageStatusStructure,
    -- | The message sender.
    sender :: Prelude.Maybe Identity,
    -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The message content.
    content :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Hides the content of a message.
    redacted :: Prelude.Maybe Prelude.Bool,
    -- | The persistence setting for a channel message.
    persistence :: Prelude.Maybe ChannelMessagePersistenceType,
    -- | The message metadata.
    metadata :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The message type.
    type' :: Prelude.Maybe ChannelMessageType,
    -- | The time at which the message was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ID of a message.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The time at which a message was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The time at which a message was edited.
    lastEditedTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'channelMessage_status' - The status of the channel message.
--
-- 'sender', 'channelMessage_sender' - The message sender.
--
-- 'channelArn', 'channelMessage_channelArn' - The ARN of the channel.
--
-- 'content', 'channelMessage_content' - The message content.
--
-- 'redacted', 'channelMessage_redacted' - Hides the content of a message.
--
-- 'persistence', 'channelMessage_persistence' - The persistence setting for a channel message.
--
-- 'metadata', 'channelMessage_metadata' - The message metadata.
--
-- 'type'', 'channelMessage_type' - The message type.
--
-- 'createdTimestamp', 'channelMessage_createdTimestamp' - The time at which the message was created.
--
-- 'messageId', 'channelMessage_messageId' - The ID of a message.
--
-- 'lastUpdatedTimestamp', 'channelMessage_lastUpdatedTimestamp' - The time at which a message was updated.
--
-- 'lastEditedTimestamp', 'channelMessage_lastEditedTimestamp' - The time at which a message was edited.
newChannelMessage ::
  ChannelMessage
newChannelMessage =
  ChannelMessage'
    { status = Prelude.Nothing,
      sender = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      content = Prelude.Nothing,
      redacted = Prelude.Nothing,
      persistence = Prelude.Nothing,
      metadata = Prelude.Nothing,
      type' = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      messageId = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      lastEditedTimestamp = Prelude.Nothing
    }

-- | The status of the channel message.
channelMessage_status :: Lens.Lens' ChannelMessage (Prelude.Maybe ChannelMessageStatusStructure)
channelMessage_status = Lens.lens (\ChannelMessage' {status} -> status) (\s@ChannelMessage' {} a -> s {status = a} :: ChannelMessage)

-- | The message sender.
channelMessage_sender :: Lens.Lens' ChannelMessage (Prelude.Maybe Identity)
channelMessage_sender = Lens.lens (\ChannelMessage' {sender} -> sender) (\s@ChannelMessage' {} a -> s {sender = a} :: ChannelMessage)

-- | The ARN of the channel.
channelMessage_channelArn :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_channelArn = Lens.lens (\ChannelMessage' {channelArn} -> channelArn) (\s@ChannelMessage' {} a -> s {channelArn = a} :: ChannelMessage)

-- | The message content.
channelMessage_content :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_content = Lens.lens (\ChannelMessage' {content} -> content) (\s@ChannelMessage' {} a -> s {content = a} :: ChannelMessage) Prelude.. Lens.mapping Core._Sensitive

-- | Hides the content of a message.
channelMessage_redacted :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Bool)
channelMessage_redacted = Lens.lens (\ChannelMessage' {redacted} -> redacted) (\s@ChannelMessage' {} a -> s {redacted = a} :: ChannelMessage)

-- | The persistence setting for a channel message.
channelMessage_persistence :: Lens.Lens' ChannelMessage (Prelude.Maybe ChannelMessagePersistenceType)
channelMessage_persistence = Lens.lens (\ChannelMessage' {persistence} -> persistence) (\s@ChannelMessage' {} a -> s {persistence = a} :: ChannelMessage)

-- | The message metadata.
channelMessage_metadata :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_metadata = Lens.lens (\ChannelMessage' {metadata} -> metadata) (\s@ChannelMessage' {} a -> s {metadata = a} :: ChannelMessage) Prelude.. Lens.mapping Core._Sensitive

-- | The message type.
channelMessage_type :: Lens.Lens' ChannelMessage (Prelude.Maybe ChannelMessageType)
channelMessage_type = Lens.lens (\ChannelMessage' {type'} -> type') (\s@ChannelMessage' {} a -> s {type' = a} :: ChannelMessage)

-- | The time at which the message was created.
channelMessage_createdTimestamp :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.UTCTime)
channelMessage_createdTimestamp = Lens.lens (\ChannelMessage' {createdTimestamp} -> createdTimestamp) (\s@ChannelMessage' {} a -> s {createdTimestamp = a} :: ChannelMessage) Prelude.. Lens.mapping Core._Time

-- | The ID of a message.
channelMessage_messageId :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.Text)
channelMessage_messageId = Lens.lens (\ChannelMessage' {messageId} -> messageId) (\s@ChannelMessage' {} a -> s {messageId = a} :: ChannelMessage)

-- | The time at which a message was updated.
channelMessage_lastUpdatedTimestamp :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.UTCTime)
channelMessage_lastUpdatedTimestamp = Lens.lens (\ChannelMessage' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ChannelMessage' {} a -> s {lastUpdatedTimestamp = a} :: ChannelMessage) Prelude.. Lens.mapping Core._Time

-- | The time at which a message was edited.
channelMessage_lastEditedTimestamp :: Lens.Lens' ChannelMessage (Prelude.Maybe Prelude.UTCTime)
channelMessage_lastEditedTimestamp = Lens.lens (\ChannelMessage' {lastEditedTimestamp} -> lastEditedTimestamp) (\s@ChannelMessage' {} a -> s {lastEditedTimestamp = a} :: ChannelMessage) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ChannelMessage where
  parseJSON =
    Core.withObject
      "ChannelMessage"
      ( \x ->
          ChannelMessage'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Sender")
            Prelude.<*> (x Core..:? "ChannelArn")
            Prelude.<*> (x Core..:? "Content")
            Prelude.<*> (x Core..:? "Redacted")
            Prelude.<*> (x Core..:? "Persistence")
            Prelude.<*> (x Core..:? "Metadata")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "MessageId")
            Prelude.<*> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "LastEditedTimestamp")
      )

instance Prelude.Hashable ChannelMessage where
  hashWithSalt salt' ChannelMessage' {..} =
    salt' `Prelude.hashWithSalt` lastEditedTimestamp
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` persistence
      `Prelude.hashWithSalt` redacted
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` sender
      `Prelude.hashWithSalt` status

instance Prelude.NFData ChannelMessage where
  rnf ChannelMessage' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastEditedTimestamp
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf persistence
      `Prelude.seq` Prelude.rnf redacted
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf sender
