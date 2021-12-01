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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelModerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelModerator where

import Amazonka.ChimeSDKMessaging.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of a channel moderator.
--
-- /See:/ 'newChannelModerator' smart constructor.
data ChannelModerator = ChannelModerator'
  { -- | The @AppInstanceUser@ who created the moderator.
    createdBy :: Prelude.Maybe Identity,
    -- | The ARN of the moderator\'s channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the moderator was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The moderator\'s data.
    moderator :: Prelude.Maybe Identity
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelModerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdBy', 'channelModerator_createdBy' - The @AppInstanceUser@ who created the moderator.
--
-- 'channelArn', 'channelModerator_channelArn' - The ARN of the moderator\'s channel.
--
-- 'createdTimestamp', 'channelModerator_createdTimestamp' - The time at which the moderator was created.
--
-- 'moderator', 'channelModerator_moderator' - The moderator\'s data.
newChannelModerator ::
  ChannelModerator
newChannelModerator =
  ChannelModerator'
    { createdBy = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      moderator = Prelude.Nothing
    }

-- | The @AppInstanceUser@ who created the moderator.
channelModerator_createdBy :: Lens.Lens' ChannelModerator (Prelude.Maybe Identity)
channelModerator_createdBy = Lens.lens (\ChannelModerator' {createdBy} -> createdBy) (\s@ChannelModerator' {} a -> s {createdBy = a} :: ChannelModerator)

-- | The ARN of the moderator\'s channel.
channelModerator_channelArn :: Lens.Lens' ChannelModerator (Prelude.Maybe Prelude.Text)
channelModerator_channelArn = Lens.lens (\ChannelModerator' {channelArn} -> channelArn) (\s@ChannelModerator' {} a -> s {channelArn = a} :: ChannelModerator)

-- | The time at which the moderator was created.
channelModerator_createdTimestamp :: Lens.Lens' ChannelModerator (Prelude.Maybe Prelude.UTCTime)
channelModerator_createdTimestamp = Lens.lens (\ChannelModerator' {createdTimestamp} -> createdTimestamp) (\s@ChannelModerator' {} a -> s {createdTimestamp = a} :: ChannelModerator) Prelude.. Lens.mapping Core._Time

-- | The moderator\'s data.
channelModerator_moderator :: Lens.Lens' ChannelModerator (Prelude.Maybe Identity)
channelModerator_moderator = Lens.lens (\ChannelModerator' {moderator} -> moderator) (\s@ChannelModerator' {} a -> s {moderator = a} :: ChannelModerator)

instance Core.FromJSON ChannelModerator where
  parseJSON =
    Core.withObject
      "ChannelModerator"
      ( \x ->
          ChannelModerator'
            Prelude.<$> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "ChannelArn")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "Moderator")
      )

instance Prelude.Hashable ChannelModerator where
  hashWithSalt salt' ChannelModerator' {..} =
    salt' `Prelude.hashWithSalt` moderator
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` createdBy

instance Prelude.NFData ChannelModerator where
  rnf ChannelModerator' {..} =
    Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf moderator
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf channelArn
