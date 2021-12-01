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
-- Module      : Amazonka.ImageBuilder.Types.ImageState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageState where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.ImageStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Image state shows the image status and the reason for that status.
--
-- /See:/ 'newImageState' smart constructor.
data ImageState = ImageState'
  { -- | The status of the image.
    status :: Prelude.Maybe ImageStatus,
    -- | The reason for the image\'s status.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'imageState_status' - The status of the image.
--
-- 'reason', 'imageState_reason' - The reason for the image\'s status.
newImageState ::
  ImageState
newImageState =
  ImageState'
    { status = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The status of the image.
imageState_status :: Lens.Lens' ImageState (Prelude.Maybe ImageStatus)
imageState_status = Lens.lens (\ImageState' {status} -> status) (\s@ImageState' {} a -> s {status = a} :: ImageState)

-- | The reason for the image\'s status.
imageState_reason :: Lens.Lens' ImageState (Prelude.Maybe Prelude.Text)
imageState_reason = Lens.lens (\ImageState' {reason} -> reason) (\s@ImageState' {} a -> s {reason = a} :: ImageState)

instance Core.FromJSON ImageState where
  parseJSON =
    Core.withObject
      "ImageState"
      ( \x ->
          ImageState'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "reason")
      )

instance Prelude.Hashable ImageState where
  hashWithSalt salt' ImageState' {..} =
    salt' `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImageState where
  rnf ImageState' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf reason
