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
-- Module      : Amazonka.Rekognition.Types.CelebrityDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.CelebrityDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.FaceDetail

-- | Information about a recognized celebrity.
--
-- /See:/ 'newCelebrityDetail' smart constructor.
data CelebrityDetail = CelebrityDetail'
  { -- | Bounding box around the body of a celebrity.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | An array of URLs pointing to additional celebrity information.
    urls :: Prelude.Maybe [Prelude.Text],
    -- | The confidence, in percentage, that Amazon Rekognition has that the
    -- recognized face is the celebrity.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The name of the celebrity.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the celebrity.
    id :: Prelude.Maybe Prelude.Text,
    -- | Face details for the recognized celebrity.
    face :: Prelude.Maybe FaceDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CelebrityDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBox', 'celebrityDetail_boundingBox' - Bounding box around the body of a celebrity.
--
-- 'urls', 'celebrityDetail_urls' - An array of URLs pointing to additional celebrity information.
--
-- 'confidence', 'celebrityDetail_confidence' - The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
--
-- 'name', 'celebrityDetail_name' - The name of the celebrity.
--
-- 'id', 'celebrityDetail_id' - The unique identifier for the celebrity.
--
-- 'face', 'celebrityDetail_face' - Face details for the recognized celebrity.
newCelebrityDetail ::
  CelebrityDetail
newCelebrityDetail =
  CelebrityDetail'
    { boundingBox = Prelude.Nothing,
      urls = Prelude.Nothing,
      confidence = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      face = Prelude.Nothing
    }

-- | Bounding box around the body of a celebrity.
celebrityDetail_boundingBox :: Lens.Lens' CelebrityDetail (Prelude.Maybe BoundingBox)
celebrityDetail_boundingBox = Lens.lens (\CelebrityDetail' {boundingBox} -> boundingBox) (\s@CelebrityDetail' {} a -> s {boundingBox = a} :: CelebrityDetail)

-- | An array of URLs pointing to additional celebrity information.
celebrityDetail_urls :: Lens.Lens' CelebrityDetail (Prelude.Maybe [Prelude.Text])
celebrityDetail_urls = Lens.lens (\CelebrityDetail' {urls} -> urls) (\s@CelebrityDetail' {} a -> s {urls = a} :: CelebrityDetail) Prelude.. Lens.mapping Lens.coerced

-- | The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
celebrityDetail_confidence :: Lens.Lens' CelebrityDetail (Prelude.Maybe Prelude.Double)
celebrityDetail_confidence = Lens.lens (\CelebrityDetail' {confidence} -> confidence) (\s@CelebrityDetail' {} a -> s {confidence = a} :: CelebrityDetail)

-- | The name of the celebrity.
celebrityDetail_name :: Lens.Lens' CelebrityDetail (Prelude.Maybe Prelude.Text)
celebrityDetail_name = Lens.lens (\CelebrityDetail' {name} -> name) (\s@CelebrityDetail' {} a -> s {name = a} :: CelebrityDetail)

-- | The unique identifier for the celebrity.
celebrityDetail_id :: Lens.Lens' CelebrityDetail (Prelude.Maybe Prelude.Text)
celebrityDetail_id = Lens.lens (\CelebrityDetail' {id} -> id) (\s@CelebrityDetail' {} a -> s {id = a} :: CelebrityDetail)

-- | Face details for the recognized celebrity.
celebrityDetail_face :: Lens.Lens' CelebrityDetail (Prelude.Maybe FaceDetail)
celebrityDetail_face = Lens.lens (\CelebrityDetail' {face} -> face) (\s@CelebrityDetail' {} a -> s {face = a} :: CelebrityDetail)

instance Core.FromJSON CelebrityDetail where
  parseJSON =
    Core.withObject
      "CelebrityDetail"
      ( \x ->
          CelebrityDetail'
            Prelude.<$> (x Core..:? "BoundingBox")
            Prelude.<*> (x Core..:? "Urls" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Face")
      )

instance Prelude.Hashable CelebrityDetail where
  hashWithSalt salt' CelebrityDetail' {..} =
    salt' `Prelude.hashWithSalt` face
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` urls
      `Prelude.hashWithSalt` boundingBox

instance Prelude.NFData CelebrityDetail where
  rnf CelebrityDetail' {..} =
    Prelude.rnf boundingBox
      `Prelude.seq` Prelude.rnf face
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf urls
