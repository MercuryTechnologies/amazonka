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
-- Module      : Amazonka.Rekognition.Types.CelebrityRecognition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.CelebrityRecognition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.CelebrityDetail

-- | Information about a detected celebrity and the time the celebrity was
-- detected in a stored video. For more information, see
-- GetCelebrityRecognition in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'newCelebrityRecognition' smart constructor.
data CelebrityRecognition = CelebrityRecognition'
  { -- | Information about a recognized celebrity.
    celebrity :: Prelude.Maybe CelebrityDetail,
    -- | The time, in milliseconds from the start of the video, that the
    -- celebrity was recognized.
    timestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CelebrityRecognition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'celebrity', 'celebrityRecognition_celebrity' - Information about a recognized celebrity.
--
-- 'timestamp', 'celebrityRecognition_timestamp' - The time, in milliseconds from the start of the video, that the
-- celebrity was recognized.
newCelebrityRecognition ::
  CelebrityRecognition
newCelebrityRecognition =
  CelebrityRecognition'
    { celebrity = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | Information about a recognized celebrity.
celebrityRecognition_celebrity :: Lens.Lens' CelebrityRecognition (Prelude.Maybe CelebrityDetail)
celebrityRecognition_celebrity = Lens.lens (\CelebrityRecognition' {celebrity} -> celebrity) (\s@CelebrityRecognition' {} a -> s {celebrity = a} :: CelebrityRecognition)

-- | The time, in milliseconds from the start of the video, that the
-- celebrity was recognized.
celebrityRecognition_timestamp :: Lens.Lens' CelebrityRecognition (Prelude.Maybe Prelude.Integer)
celebrityRecognition_timestamp = Lens.lens (\CelebrityRecognition' {timestamp} -> timestamp) (\s@CelebrityRecognition' {} a -> s {timestamp = a} :: CelebrityRecognition)

instance Core.FromJSON CelebrityRecognition where
  parseJSON =
    Core.withObject
      "CelebrityRecognition"
      ( \x ->
          CelebrityRecognition'
            Prelude.<$> (x Core..:? "Celebrity")
            Prelude.<*> (x Core..:? "Timestamp")
      )

instance Prelude.Hashable CelebrityRecognition where
  hashWithSalt salt' CelebrityRecognition' {..} =
    salt' `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` celebrity

instance Prelude.NFData CelebrityRecognition where
  rnf CelebrityRecognition' {..} =
    Prelude.rnf celebrity
      `Prelude.seq` Prelude.rnf timestamp
