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
-- Module      : Amazonka.MediaLive.Types.OutputDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.OutputDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for OutputDestinationSettings
--
-- /See:/ 'newOutputDestinationSettings' smart constructor.
data OutputDestinationSettings = OutputDestinationSettings'
  { -- | A URL specifying a destination
    url :: Prelude.Maybe Prelude.Text,
    -- | username for destination
    username :: Prelude.Maybe Prelude.Text,
    -- | key used to extract the password from EC2 Parameter store
    passwordParam :: Prelude.Maybe Prelude.Text,
    -- | Stream name for RTMP destinations (URLs of type rtmp:\/\/)
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'outputDestinationSettings_url' - A URL specifying a destination
--
-- 'username', 'outputDestinationSettings_username' - username for destination
--
-- 'passwordParam', 'outputDestinationSettings_passwordParam' - key used to extract the password from EC2 Parameter store
--
-- 'streamName', 'outputDestinationSettings_streamName' - Stream name for RTMP destinations (URLs of type rtmp:\/\/)
newOutputDestinationSettings ::
  OutputDestinationSettings
newOutputDestinationSettings =
  OutputDestinationSettings'
    { url = Prelude.Nothing,
      username = Prelude.Nothing,
      passwordParam = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | A URL specifying a destination
outputDestinationSettings_url :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_url = Lens.lens (\OutputDestinationSettings' {url} -> url) (\s@OutputDestinationSettings' {} a -> s {url = a} :: OutputDestinationSettings)

-- | username for destination
outputDestinationSettings_username :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_username = Lens.lens (\OutputDestinationSettings' {username} -> username) (\s@OutputDestinationSettings' {} a -> s {username = a} :: OutputDestinationSettings)

-- | key used to extract the password from EC2 Parameter store
outputDestinationSettings_passwordParam :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_passwordParam = Lens.lens (\OutputDestinationSettings' {passwordParam} -> passwordParam) (\s@OutputDestinationSettings' {} a -> s {passwordParam = a} :: OutputDestinationSettings)

-- | Stream name for RTMP destinations (URLs of type rtmp:\/\/)
outputDestinationSettings_streamName :: Lens.Lens' OutputDestinationSettings (Prelude.Maybe Prelude.Text)
outputDestinationSettings_streamName = Lens.lens (\OutputDestinationSettings' {streamName} -> streamName) (\s@OutputDestinationSettings' {} a -> s {streamName = a} :: OutputDestinationSettings)

instance Core.FromJSON OutputDestinationSettings where
  parseJSON =
    Core.withObject
      "OutputDestinationSettings"
      ( \x ->
          OutputDestinationSettings'
            Prelude.<$> (x Core..:? "url")
            Prelude.<*> (x Core..:? "username")
            Prelude.<*> (x Core..:? "passwordParam")
            Prelude.<*> (x Core..:? "streamName")
      )

instance Prelude.Hashable OutputDestinationSettings where
  hashWithSalt salt' OutputDestinationSettings' {..} =
    salt' `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` passwordParam
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` url

instance Prelude.NFData OutputDestinationSettings where
  rnf OutputDestinationSettings' {..} =
    Prelude.rnf url
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf passwordParam
      `Prelude.seq` Prelude.rnf username

instance Core.ToJSON OutputDestinationSettings where
  toJSON OutputDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("url" Core..=) Prelude.<$> url,
            ("username" Core..=) Prelude.<$> username,
            ("passwordParam" Core..=) Prelude.<$> passwordParam,
            ("streamName" Core..=) Prelude.<$> streamName
          ]
      )
