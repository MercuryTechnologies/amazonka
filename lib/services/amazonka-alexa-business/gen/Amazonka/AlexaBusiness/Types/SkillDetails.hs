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
-- Module      : Amazonka.AlexaBusiness.Types.SkillDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.SkillDetails where

import Amazonka.AlexaBusiness.Types.DeveloperInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Granular information about the skill.
--
-- /See:/ 'newSkillDetails' smart constructor.
data SkillDetails = SkillDetails'
  { -- | The types of skills.
    skillTypes :: Prelude.Maybe [Prelude.Text],
    -- | The description of the product.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | The phrase used to trigger the skill.
    invocationPhrase :: Prelude.Maybe Prelude.Text,
    -- | The details about the developer that published the skill.
    developerInfo :: Prelude.Maybe DeveloperInfo,
    -- | The URL of the end user license agreement.
    endUserLicenseAgreement :: Prelude.Maybe Prelude.Text,
    -- | The generic keywords associated with the skill that can be used to find
    -- a skill.
    genericKeywords :: Prelude.Maybe [Prelude.Text],
    -- | /This member has been deprecated./
    --
    -- The list of reviews for the skill, including Key and Value pair.
    reviews :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date when the skill was released.
    releaseDate :: Prelude.Maybe Prelude.Text,
    -- | The updates added in bullet points.
    newInThisVersionBulletPoints' :: Prelude.Maybe [Prelude.Text],
    -- | The details about what the skill supports organized as bullet points.
    bulletPoints :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SkillDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillTypes', 'skillDetails_skillTypes' - The types of skills.
--
-- 'productDescription', 'skillDetails_productDescription' - The description of the product.
--
-- 'invocationPhrase', 'skillDetails_invocationPhrase' - The phrase used to trigger the skill.
--
-- 'developerInfo', 'skillDetails_developerInfo' - The details about the developer that published the skill.
--
-- 'endUserLicenseAgreement', 'skillDetails_endUserLicenseAgreement' - The URL of the end user license agreement.
--
-- 'genericKeywords', 'skillDetails_genericKeywords' - The generic keywords associated with the skill that can be used to find
-- a skill.
--
-- 'reviews', 'skillDetails_reviews' - /This member has been deprecated./
--
-- The list of reviews for the skill, including Key and Value pair.
--
-- 'releaseDate', 'skillDetails_releaseDate' - The date when the skill was released.
--
-- 'newInThisVersionBulletPoints'', 'skillDetails_newInThisVersionBulletPoints' - The updates added in bullet points.
--
-- 'bulletPoints', 'skillDetails_bulletPoints' - The details about what the skill supports organized as bullet points.
newSkillDetails ::
  SkillDetails
newSkillDetails =
  SkillDetails'
    { skillTypes = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      invocationPhrase = Prelude.Nothing,
      developerInfo = Prelude.Nothing,
      endUserLicenseAgreement = Prelude.Nothing,
      genericKeywords = Prelude.Nothing,
      reviews = Prelude.Nothing,
      releaseDate = Prelude.Nothing,
      newInThisVersionBulletPoints' = Prelude.Nothing,
      bulletPoints = Prelude.Nothing
    }

-- | The types of skills.
skillDetails_skillTypes :: Lens.Lens' SkillDetails (Prelude.Maybe [Prelude.Text])
skillDetails_skillTypes = Lens.lens (\SkillDetails' {skillTypes} -> skillTypes) (\s@SkillDetails' {} a -> s {skillTypes = a} :: SkillDetails) Prelude.. Lens.mapping Lens.coerced

-- | The description of the product.
skillDetails_productDescription :: Lens.Lens' SkillDetails (Prelude.Maybe Prelude.Text)
skillDetails_productDescription = Lens.lens (\SkillDetails' {productDescription} -> productDescription) (\s@SkillDetails' {} a -> s {productDescription = a} :: SkillDetails)

-- | The phrase used to trigger the skill.
skillDetails_invocationPhrase :: Lens.Lens' SkillDetails (Prelude.Maybe Prelude.Text)
skillDetails_invocationPhrase = Lens.lens (\SkillDetails' {invocationPhrase} -> invocationPhrase) (\s@SkillDetails' {} a -> s {invocationPhrase = a} :: SkillDetails)

-- | The details about the developer that published the skill.
skillDetails_developerInfo :: Lens.Lens' SkillDetails (Prelude.Maybe DeveloperInfo)
skillDetails_developerInfo = Lens.lens (\SkillDetails' {developerInfo} -> developerInfo) (\s@SkillDetails' {} a -> s {developerInfo = a} :: SkillDetails)

-- | The URL of the end user license agreement.
skillDetails_endUserLicenseAgreement :: Lens.Lens' SkillDetails (Prelude.Maybe Prelude.Text)
skillDetails_endUserLicenseAgreement = Lens.lens (\SkillDetails' {endUserLicenseAgreement} -> endUserLicenseAgreement) (\s@SkillDetails' {} a -> s {endUserLicenseAgreement = a} :: SkillDetails)

-- | The generic keywords associated with the skill that can be used to find
-- a skill.
skillDetails_genericKeywords :: Lens.Lens' SkillDetails (Prelude.Maybe [Prelude.Text])
skillDetails_genericKeywords = Lens.lens (\SkillDetails' {genericKeywords} -> genericKeywords) (\s@SkillDetails' {} a -> s {genericKeywords = a} :: SkillDetails) Prelude.. Lens.mapping Lens.coerced

-- | /This member has been deprecated./
--
-- The list of reviews for the skill, including Key and Value pair.
skillDetails_reviews :: Lens.Lens' SkillDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
skillDetails_reviews = Lens.lens (\SkillDetails' {reviews} -> reviews) (\s@SkillDetails' {} a -> s {reviews = a} :: SkillDetails) Prelude.. Lens.mapping Lens.coerced

-- | The date when the skill was released.
skillDetails_releaseDate :: Lens.Lens' SkillDetails (Prelude.Maybe Prelude.Text)
skillDetails_releaseDate = Lens.lens (\SkillDetails' {releaseDate} -> releaseDate) (\s@SkillDetails' {} a -> s {releaseDate = a} :: SkillDetails)

-- | The updates added in bullet points.
skillDetails_newInThisVersionBulletPoints :: Lens.Lens' SkillDetails (Prelude.Maybe [Prelude.Text])
skillDetails_newInThisVersionBulletPoints = Lens.lens (\SkillDetails' {newInThisVersionBulletPoints'} -> newInThisVersionBulletPoints') (\s@SkillDetails' {} a -> s {newInThisVersionBulletPoints' = a} :: SkillDetails) Prelude.. Lens.mapping Lens.coerced

-- | The details about what the skill supports organized as bullet points.
skillDetails_bulletPoints :: Lens.Lens' SkillDetails (Prelude.Maybe [Prelude.Text])
skillDetails_bulletPoints = Lens.lens (\SkillDetails' {bulletPoints} -> bulletPoints) (\s@SkillDetails' {} a -> s {bulletPoints = a} :: SkillDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SkillDetails where
  parseJSON =
    Core.withObject
      "SkillDetails"
      ( \x ->
          SkillDetails'
            Prelude.<$> (x Core..:? "SkillTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ProductDescription")
            Prelude.<*> (x Core..:? "InvocationPhrase")
            Prelude.<*> (x Core..:? "DeveloperInfo")
            Prelude.<*> (x Core..:? "EndUserLicenseAgreement")
            Prelude.<*> ( x Core..:? "GenericKeywords"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Reviews" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ReleaseDate")
            Prelude.<*> ( x Core..:? "NewInThisVersionBulletPoints"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "BulletPoints" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SkillDetails where
  hashWithSalt salt' SkillDetails' {..} =
    salt' `Prelude.hashWithSalt` bulletPoints
      `Prelude.hashWithSalt` newInThisVersionBulletPoints'
      `Prelude.hashWithSalt` releaseDate
      `Prelude.hashWithSalt` reviews
      `Prelude.hashWithSalt` genericKeywords
      `Prelude.hashWithSalt` endUserLicenseAgreement
      `Prelude.hashWithSalt` developerInfo
      `Prelude.hashWithSalt` invocationPhrase
      `Prelude.hashWithSalt` productDescription
      `Prelude.hashWithSalt` skillTypes

instance Prelude.NFData SkillDetails where
  rnf SkillDetails' {..} =
    Prelude.rnf skillTypes
      `Prelude.seq` Prelude.rnf bulletPoints
      `Prelude.seq` Prelude.rnf newInThisVersionBulletPoints'
      `Prelude.seq` Prelude.rnf releaseDate
      `Prelude.seq` Prelude.rnf reviews
      `Prelude.seq` Prelude.rnf genericKeywords
      `Prelude.seq` Prelude.rnf endUserLicenseAgreement
      `Prelude.seq` Prelude.rnf developerInfo
      `Prelude.seq` Prelude.rnf invocationPhrase
      `Prelude.seq` Prelude.rnf productDescription
