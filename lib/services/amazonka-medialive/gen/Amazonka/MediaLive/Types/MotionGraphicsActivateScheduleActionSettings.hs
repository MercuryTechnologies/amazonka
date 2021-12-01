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
-- Module      : Amazonka.MediaLive.Types.MotionGraphicsActivateScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MotionGraphicsActivateScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings to specify the rendering of motion graphics into the video
-- stream.
--
-- /See:/ 'newMotionGraphicsActivateScheduleActionSettings' smart constructor.
data MotionGraphicsActivateScheduleActionSettings = MotionGraphicsActivateScheduleActionSettings'
  { -- | URI of the HTML5 content to be rendered into the live stream.
    url :: Prelude.Maybe Prelude.Text,
    -- | Documentation update needed
    username :: Prelude.Maybe Prelude.Text,
    -- | Key used to extract the password from EC2 Parameter store
    passwordParam :: Prelude.Maybe Prelude.Text,
    -- | Duration (in milliseconds) that motion graphics should render on to the
    -- video stream. Leaving out this property or setting to 0 will result in
    -- rendering continuing until a deactivate action is processed.
    duration :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MotionGraphicsActivateScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'motionGraphicsActivateScheduleActionSettings_url' - URI of the HTML5 content to be rendered into the live stream.
--
-- 'username', 'motionGraphicsActivateScheduleActionSettings_username' - Documentation update needed
--
-- 'passwordParam', 'motionGraphicsActivateScheduleActionSettings_passwordParam' - Key used to extract the password from EC2 Parameter store
--
-- 'duration', 'motionGraphicsActivateScheduleActionSettings_duration' - Duration (in milliseconds) that motion graphics should render on to the
-- video stream. Leaving out this property or setting to 0 will result in
-- rendering continuing until a deactivate action is processed.
newMotionGraphicsActivateScheduleActionSettings ::
  MotionGraphicsActivateScheduleActionSettings
newMotionGraphicsActivateScheduleActionSettings =
  MotionGraphicsActivateScheduleActionSettings'
    { url =
        Prelude.Nothing,
      username = Prelude.Nothing,
      passwordParam =
        Prelude.Nothing,
      duration = Prelude.Nothing
    }

-- | URI of the HTML5 content to be rendered into the live stream.
motionGraphicsActivateScheduleActionSettings_url :: Lens.Lens' MotionGraphicsActivateScheduleActionSettings (Prelude.Maybe Prelude.Text)
motionGraphicsActivateScheduleActionSettings_url = Lens.lens (\MotionGraphicsActivateScheduleActionSettings' {url} -> url) (\s@MotionGraphicsActivateScheduleActionSettings' {} a -> s {url = a} :: MotionGraphicsActivateScheduleActionSettings)

-- | Documentation update needed
motionGraphicsActivateScheduleActionSettings_username :: Lens.Lens' MotionGraphicsActivateScheduleActionSettings (Prelude.Maybe Prelude.Text)
motionGraphicsActivateScheduleActionSettings_username = Lens.lens (\MotionGraphicsActivateScheduleActionSettings' {username} -> username) (\s@MotionGraphicsActivateScheduleActionSettings' {} a -> s {username = a} :: MotionGraphicsActivateScheduleActionSettings)

-- | Key used to extract the password from EC2 Parameter store
motionGraphicsActivateScheduleActionSettings_passwordParam :: Lens.Lens' MotionGraphicsActivateScheduleActionSettings (Prelude.Maybe Prelude.Text)
motionGraphicsActivateScheduleActionSettings_passwordParam = Lens.lens (\MotionGraphicsActivateScheduleActionSettings' {passwordParam} -> passwordParam) (\s@MotionGraphicsActivateScheduleActionSettings' {} a -> s {passwordParam = a} :: MotionGraphicsActivateScheduleActionSettings)

-- | Duration (in milliseconds) that motion graphics should render on to the
-- video stream. Leaving out this property or setting to 0 will result in
-- rendering continuing until a deactivate action is processed.
motionGraphicsActivateScheduleActionSettings_duration :: Lens.Lens' MotionGraphicsActivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
motionGraphicsActivateScheduleActionSettings_duration = Lens.lens (\MotionGraphicsActivateScheduleActionSettings' {duration} -> duration) (\s@MotionGraphicsActivateScheduleActionSettings' {} a -> s {duration = a} :: MotionGraphicsActivateScheduleActionSettings)

instance
  Core.FromJSON
    MotionGraphicsActivateScheduleActionSettings
  where
  parseJSON =
    Core.withObject
      "MotionGraphicsActivateScheduleActionSettings"
      ( \x ->
          MotionGraphicsActivateScheduleActionSettings'
            Prelude.<$> (x Core..:? "url")
              Prelude.<*> (x Core..:? "username")
              Prelude.<*> (x Core..:? "passwordParam")
              Prelude.<*> (x Core..:? "duration")
      )

instance
  Prelude.Hashable
    MotionGraphicsActivateScheduleActionSettings
  where
  hashWithSalt
    salt'
    MotionGraphicsActivateScheduleActionSettings' {..} =
      salt' `Prelude.hashWithSalt` duration
        `Prelude.hashWithSalt` passwordParam
        `Prelude.hashWithSalt` username
        `Prelude.hashWithSalt` url

instance
  Prelude.NFData
    MotionGraphicsActivateScheduleActionSettings
  where
  rnf MotionGraphicsActivateScheduleActionSettings' {..} =
    Prelude.rnf url `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf passwordParam
      `Prelude.seq` Prelude.rnf username

instance
  Core.ToJSON
    MotionGraphicsActivateScheduleActionSettings
  where
  toJSON
    MotionGraphicsActivateScheduleActionSettings' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("url" Core..=) Prelude.<$> url,
              ("username" Core..=) Prelude.<$> username,
              ("passwordParam" Core..=) Prelude.<$> passwordParam,
              ("duration" Core..=) Prelude.<$> duration
            ]
        )
