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
-- Module      : Amazonka.Chime.Types.Meeting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Meeting where

import Amazonka.Chime.Types.MediaPlacement
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A meeting created using the Amazon Chime SDK.
--
-- /See:/ 'newMeeting' smart constructor.
data Meeting = Meeting'
  { -- | The Region in which you create the meeting. Available values:
    -- @af-south-1@, @ap-northeast-1@, @ap-northeast-2@, @ap-south-1@,
    -- @ap-southeast-1@, @ap-southeast-2@, @ca-central-1@, @eu-central-1@,
    -- @eu-north-1@, @eu-south-1@, @eu-west-1@, @eu-west-2@, @eu-west-3@,
    -- @sa-east-1@, @us-east-1@, @us-east-2@, @us-west-1@, @us-west-2@.
    mediaRegion :: Prelude.Maybe Prelude.Text,
    -- | The media placement for the meeting.
    mediaPlacement :: Prelude.Maybe MediaPlacement,
    -- | The external meeting ID.
    externalMeetingId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Meeting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaRegion', 'meeting_mediaRegion' - The Region in which you create the meeting. Available values:
-- @af-south-1@, @ap-northeast-1@, @ap-northeast-2@, @ap-south-1@,
-- @ap-southeast-1@, @ap-southeast-2@, @ca-central-1@, @eu-central-1@,
-- @eu-north-1@, @eu-south-1@, @eu-west-1@, @eu-west-2@, @eu-west-3@,
-- @sa-east-1@, @us-east-1@, @us-east-2@, @us-west-1@, @us-west-2@.
--
-- 'mediaPlacement', 'meeting_mediaPlacement' - The media placement for the meeting.
--
-- 'externalMeetingId', 'meeting_externalMeetingId' - The external meeting ID.
--
-- 'meetingId', 'meeting_meetingId' - The Amazon Chime SDK meeting ID.
newMeeting ::
  Meeting
newMeeting =
  Meeting'
    { mediaRegion = Prelude.Nothing,
      mediaPlacement = Prelude.Nothing,
      externalMeetingId = Prelude.Nothing,
      meetingId = Prelude.Nothing
    }

-- | The Region in which you create the meeting. Available values:
-- @af-south-1@, @ap-northeast-1@, @ap-northeast-2@, @ap-south-1@,
-- @ap-southeast-1@, @ap-southeast-2@, @ca-central-1@, @eu-central-1@,
-- @eu-north-1@, @eu-south-1@, @eu-west-1@, @eu-west-2@, @eu-west-3@,
-- @sa-east-1@, @us-east-1@, @us-east-2@, @us-west-1@, @us-west-2@.
meeting_mediaRegion :: Lens.Lens' Meeting (Prelude.Maybe Prelude.Text)
meeting_mediaRegion = Lens.lens (\Meeting' {mediaRegion} -> mediaRegion) (\s@Meeting' {} a -> s {mediaRegion = a} :: Meeting)

-- | The media placement for the meeting.
meeting_mediaPlacement :: Lens.Lens' Meeting (Prelude.Maybe MediaPlacement)
meeting_mediaPlacement = Lens.lens (\Meeting' {mediaPlacement} -> mediaPlacement) (\s@Meeting' {} a -> s {mediaPlacement = a} :: Meeting)

-- | The external meeting ID.
meeting_externalMeetingId :: Lens.Lens' Meeting (Prelude.Maybe Prelude.Text)
meeting_externalMeetingId = Lens.lens (\Meeting' {externalMeetingId} -> externalMeetingId) (\s@Meeting' {} a -> s {externalMeetingId = a} :: Meeting) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Chime SDK meeting ID.
meeting_meetingId :: Lens.Lens' Meeting (Prelude.Maybe Prelude.Text)
meeting_meetingId = Lens.lens (\Meeting' {meetingId} -> meetingId) (\s@Meeting' {} a -> s {meetingId = a} :: Meeting)

instance Core.FromJSON Meeting where
  parseJSON =
    Core.withObject
      "Meeting"
      ( \x ->
          Meeting'
            Prelude.<$> (x Core..:? "MediaRegion")
            Prelude.<*> (x Core..:? "MediaPlacement")
            Prelude.<*> (x Core..:? "ExternalMeetingId")
            Prelude.<*> (x Core..:? "MeetingId")
      )

instance Prelude.Hashable Meeting where
  hashWithSalt salt' Meeting' {..} =
    salt' `Prelude.hashWithSalt` meetingId
      `Prelude.hashWithSalt` externalMeetingId
      `Prelude.hashWithSalt` mediaPlacement
      `Prelude.hashWithSalt` mediaRegion

instance Prelude.NFData Meeting where
  rnf Meeting' {..} =
    Prelude.rnf mediaRegion
      `Prelude.seq` Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf externalMeetingId
      `Prelude.seq` Prelude.rnf mediaPlacement
