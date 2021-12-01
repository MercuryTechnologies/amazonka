{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LakeFormation.GetResourceLFTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tags applied to a resource.
module Amazonka.LakeFormation.GetResourceLFTags
  ( -- * Creating a Request
    GetResourceLFTags (..),
    newGetResourceLFTags,

    -- * Request Lenses
    getResourceLFTags_showAssignedLFTags,
    getResourceLFTags_catalogId,
    getResourceLFTags_resource,

    -- * Destructuring the Response
    GetResourceLFTagsResponse (..),
    newGetResourceLFTagsResponse,

    -- * Response Lenses
    getResourceLFTagsResponse_lFTagsOnTable,
    getResourceLFTagsResponse_lFTagOnDatabase,
    getResourceLFTagsResponse_lFTagsOnColumns,
    getResourceLFTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.LakeFormation.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceLFTags' smart constructor.
data GetResourceLFTags = GetResourceLFTags'
  { -- | Indicates whether to show the assigned tags.
    showAssignedLFTags :: Prelude.Maybe Prelude.Bool,
    -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your AWS Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The resource for which you want to return tags.
    resource :: Resource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceLFTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'showAssignedLFTags', 'getResourceLFTags_showAssignedLFTags' - Indicates whether to show the assigned tags.
--
-- 'catalogId', 'getResourceLFTags_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
--
-- 'resource', 'getResourceLFTags_resource' - The resource for which you want to return tags.
newGetResourceLFTags ::
  -- | 'resource'
  Resource ->
  GetResourceLFTags
newGetResourceLFTags pResource_ =
  GetResourceLFTags'
    { showAssignedLFTags =
        Prelude.Nothing,
      catalogId = Prelude.Nothing,
      resource = pResource_
    }

-- | Indicates whether to show the assigned tags.
getResourceLFTags_showAssignedLFTags :: Lens.Lens' GetResourceLFTags (Prelude.Maybe Prelude.Bool)
getResourceLFTags_showAssignedLFTags = Lens.lens (\GetResourceLFTags' {showAssignedLFTags} -> showAssignedLFTags) (\s@GetResourceLFTags' {} a -> s {showAssignedLFTags = a} :: GetResourceLFTags)

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
getResourceLFTags_catalogId :: Lens.Lens' GetResourceLFTags (Prelude.Maybe Prelude.Text)
getResourceLFTags_catalogId = Lens.lens (\GetResourceLFTags' {catalogId} -> catalogId) (\s@GetResourceLFTags' {} a -> s {catalogId = a} :: GetResourceLFTags)

-- | The resource for which you want to return tags.
getResourceLFTags_resource :: Lens.Lens' GetResourceLFTags Resource
getResourceLFTags_resource = Lens.lens (\GetResourceLFTags' {resource} -> resource) (\s@GetResourceLFTags' {} a -> s {resource = a} :: GetResourceLFTags)

instance Core.AWSRequest GetResourceLFTags where
  type
    AWSResponse GetResourceLFTags =
      GetResourceLFTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceLFTagsResponse'
            Prelude.<$> (x Core..?> "LFTagsOnTable")
            Prelude.<*> (x Core..?> "LFTagOnDatabase")
            Prelude.<*> ( x Core..?> "LFTagsOnColumns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceLFTags where
  hashWithSalt salt' GetResourceLFTags' {..} =
    salt' `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` showAssignedLFTags

instance Prelude.NFData GetResourceLFTags where
  rnf GetResourceLFTags' {..} =
    Prelude.rnf showAssignedLFTags
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf catalogId

instance Core.ToHeaders GetResourceLFTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLakeFormation.GetResourceLFTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResourceLFTags where
  toJSON GetResourceLFTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ShowAssignedLFTags" Core..=)
              Prelude.<$> showAssignedLFTags,
            ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("Resource" Core..= resource)
          ]
      )

instance Core.ToPath GetResourceLFTags where
  toPath = Prelude.const "/"

instance Core.ToQuery GetResourceLFTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceLFTagsResponse' smart constructor.
data GetResourceLFTagsResponse = GetResourceLFTagsResponse'
  { -- | A list of tags applied to a table resource.
    lFTagsOnTable :: Prelude.Maybe (Prelude.NonEmpty LFTagPair),
    -- | A list of tags applied to a database resource.
    lFTagOnDatabase :: Prelude.Maybe (Prelude.NonEmpty LFTagPair),
    -- | A list of tags applied to a column resource.
    lFTagsOnColumns :: Prelude.Maybe [ColumnLFTag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceLFTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lFTagsOnTable', 'getResourceLFTagsResponse_lFTagsOnTable' - A list of tags applied to a table resource.
--
-- 'lFTagOnDatabase', 'getResourceLFTagsResponse_lFTagOnDatabase' - A list of tags applied to a database resource.
--
-- 'lFTagsOnColumns', 'getResourceLFTagsResponse_lFTagsOnColumns' - A list of tags applied to a column resource.
--
-- 'httpStatus', 'getResourceLFTagsResponse_httpStatus' - The response's http status code.
newGetResourceLFTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceLFTagsResponse
newGetResourceLFTagsResponse pHttpStatus_ =
  GetResourceLFTagsResponse'
    { lFTagsOnTable =
        Prelude.Nothing,
      lFTagOnDatabase = Prelude.Nothing,
      lFTagsOnColumns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tags applied to a table resource.
getResourceLFTagsResponse_lFTagsOnTable :: Lens.Lens' GetResourceLFTagsResponse (Prelude.Maybe (Prelude.NonEmpty LFTagPair))
getResourceLFTagsResponse_lFTagsOnTable = Lens.lens (\GetResourceLFTagsResponse' {lFTagsOnTable} -> lFTagsOnTable) (\s@GetResourceLFTagsResponse' {} a -> s {lFTagsOnTable = a} :: GetResourceLFTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags applied to a database resource.
getResourceLFTagsResponse_lFTagOnDatabase :: Lens.Lens' GetResourceLFTagsResponse (Prelude.Maybe (Prelude.NonEmpty LFTagPair))
getResourceLFTagsResponse_lFTagOnDatabase = Lens.lens (\GetResourceLFTagsResponse' {lFTagOnDatabase} -> lFTagOnDatabase) (\s@GetResourceLFTagsResponse' {} a -> s {lFTagOnDatabase = a} :: GetResourceLFTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags applied to a column resource.
getResourceLFTagsResponse_lFTagsOnColumns :: Lens.Lens' GetResourceLFTagsResponse (Prelude.Maybe [ColumnLFTag])
getResourceLFTagsResponse_lFTagsOnColumns = Lens.lens (\GetResourceLFTagsResponse' {lFTagsOnColumns} -> lFTagsOnColumns) (\s@GetResourceLFTagsResponse' {} a -> s {lFTagsOnColumns = a} :: GetResourceLFTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getResourceLFTagsResponse_httpStatus :: Lens.Lens' GetResourceLFTagsResponse Prelude.Int
getResourceLFTagsResponse_httpStatus = Lens.lens (\GetResourceLFTagsResponse' {httpStatus} -> httpStatus) (\s@GetResourceLFTagsResponse' {} a -> s {httpStatus = a} :: GetResourceLFTagsResponse)

instance Prelude.NFData GetResourceLFTagsResponse where
  rnf GetResourceLFTagsResponse' {..} =
    Prelude.rnf lFTagsOnTable
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf lFTagsOnColumns
      `Prelude.seq` Prelude.rnf lFTagOnDatabase
