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
-- Module      : Amazonka.IoTSiteWise.Types.Image
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Image where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types.ImageFile
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains an image that is one of the following:
--
-- -   An image file. Choose this option to upload a new image.
--
-- -   The ID of an existing image. Choose this option to keep an existing
--     image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The ID of an existing image. Specify this parameter to keep an existing
    -- image.
    id :: Prelude.Maybe Prelude.Text,
    file :: Prelude.Maybe ImageFile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'image_id' - The ID of an existing image. Specify this parameter to keep an existing
-- image.
--
-- 'file', 'image_file' - Undocumented member.
newImage ::
  Image
newImage =
  Image'
    { id = Prelude.Nothing,
      file = Prelude.Nothing
    }

-- | The ID of an existing image. Specify this parameter to keep an existing
-- image.
image_id :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_id = Lens.lens (\Image' {id} -> id) (\s@Image' {} a -> s {id = a} :: Image)

-- | Undocumented member.
image_file :: Lens.Lens' Image (Prelude.Maybe ImageFile)
image_file = Lens.lens (\Image' {file} -> file) (\s@Image' {} a -> s {file = a} :: Image)

instance Prelude.Hashable Image where
  hashWithSalt salt' Image' {..} =
    salt' `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` id

instance Prelude.NFData Image where
  rnf Image' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf file

instance Core.ToJSON Image where
  toJSON Image' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("id" Core..=) Prelude.<$> id,
            ("file" Core..=) Prelude.<$> file
          ]
      )
