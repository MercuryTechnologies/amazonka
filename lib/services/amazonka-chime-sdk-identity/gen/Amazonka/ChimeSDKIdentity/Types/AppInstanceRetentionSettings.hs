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
-- Module      : Amazonka.ChimeSDKIdentity.Types.AppInstanceRetentionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.AppInstanceRetentionSettings where

import Amazonka.ChimeSDKIdentity.Types.ChannelRetentionSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of the data-retention settings for an @AppInstance@.
--
-- /See:/ 'newAppInstanceRetentionSettings' smart constructor.
data AppInstanceRetentionSettings = AppInstanceRetentionSettings'
  { -- | The length of time in days to retain the messages in a channel.
    channelRetentionSettings :: Prelude.Maybe ChannelRetentionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceRetentionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelRetentionSettings', 'appInstanceRetentionSettings_channelRetentionSettings' - The length of time in days to retain the messages in a channel.
newAppInstanceRetentionSettings ::
  AppInstanceRetentionSettings
newAppInstanceRetentionSettings =
  AppInstanceRetentionSettings'
    { channelRetentionSettings =
        Prelude.Nothing
    }

-- | The length of time in days to retain the messages in a channel.
appInstanceRetentionSettings_channelRetentionSettings :: Lens.Lens' AppInstanceRetentionSettings (Prelude.Maybe ChannelRetentionSettings)
appInstanceRetentionSettings_channelRetentionSettings = Lens.lens (\AppInstanceRetentionSettings' {channelRetentionSettings} -> channelRetentionSettings) (\s@AppInstanceRetentionSettings' {} a -> s {channelRetentionSettings = a} :: AppInstanceRetentionSettings)

instance Core.FromJSON AppInstanceRetentionSettings where
  parseJSON =
    Core.withObject
      "AppInstanceRetentionSettings"
      ( \x ->
          AppInstanceRetentionSettings'
            Prelude.<$> (x Core..:? "ChannelRetentionSettings")
      )

instance
  Prelude.Hashable
    AppInstanceRetentionSettings
  where
  hashWithSalt salt' AppInstanceRetentionSettings' {..} =
    salt'
      `Prelude.hashWithSalt` channelRetentionSettings

instance Prelude.NFData AppInstanceRetentionSettings where
  rnf AppInstanceRetentionSettings' {..} =
    Prelude.rnf channelRetentionSettings

instance Core.ToJSON AppInstanceRetentionSettings where
  toJSON AppInstanceRetentionSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ChannelRetentionSettings" Core..=)
              Prelude.<$> channelRetentionSettings
          ]
      )
