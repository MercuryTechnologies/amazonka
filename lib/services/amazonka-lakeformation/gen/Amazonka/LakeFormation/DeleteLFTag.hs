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
-- Module      : Amazonka.LakeFormation.DeleteLFTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tag key name. If the attribute key does not exist
-- or the tag does not exist, then the operation will not do anything. If
-- the attribute key exists, then the operation checks if any resources are
-- tagged with this attribute key, if yes, the API throws a 400 Exception
-- with the message \"Delete not allowed\" as the tag key is still attached
-- with resources. You can consider untagging resources with this tag key.
module Amazonka.LakeFormation.DeleteLFTag
  ( -- * Creating a Request
    DeleteLFTag (..),
    newDeleteLFTag,

    -- * Request Lenses
    deleteLFTag_catalogId,
    deleteLFTag_tagKey,

    -- * Destructuring the Response
    DeleteLFTagResponse (..),
    newDeleteLFTagResponse,

    -- * Response Lenses
    deleteLFTagResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.LakeFormation.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLFTag' smart constructor.
data DeleteLFTag = DeleteLFTag'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your AWS Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The key-name for the tag to delete.
    tagKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLFTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteLFTag_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
--
-- 'tagKey', 'deleteLFTag_tagKey' - The key-name for the tag to delete.
newDeleteLFTag ::
  -- | 'tagKey'
  Prelude.Text ->
  DeleteLFTag
newDeleteLFTag pTagKey_ =
  DeleteLFTag'
    { catalogId = Prelude.Nothing,
      tagKey = pTagKey_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
deleteLFTag_catalogId :: Lens.Lens' DeleteLFTag (Prelude.Maybe Prelude.Text)
deleteLFTag_catalogId = Lens.lens (\DeleteLFTag' {catalogId} -> catalogId) (\s@DeleteLFTag' {} a -> s {catalogId = a} :: DeleteLFTag)

-- | The key-name for the tag to delete.
deleteLFTag_tagKey :: Lens.Lens' DeleteLFTag Prelude.Text
deleteLFTag_tagKey = Lens.lens (\DeleteLFTag' {tagKey} -> tagKey) (\s@DeleteLFTag' {} a -> s {tagKey = a} :: DeleteLFTag)

instance Core.AWSRequest DeleteLFTag where
  type AWSResponse DeleteLFTag = DeleteLFTagResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLFTagResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLFTag where
  hashWithSalt salt' DeleteLFTag' {..} =
    salt' `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` catalogId

instance Prelude.NFData DeleteLFTag where
  rnf DeleteLFTag' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tagKey

instance Core.ToHeaders DeleteLFTag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLakeFormation.DeleteLFTag" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteLFTag where
  toJSON DeleteLFTag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("TagKey" Core..= tagKey)
          ]
      )

instance Core.ToPath DeleteLFTag where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLFTag where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLFTagResponse' smart constructor.
data DeleteLFTagResponse = DeleteLFTagResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLFTagResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLFTagResponse_httpStatus' - The response's http status code.
newDeleteLFTagResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLFTagResponse
newDeleteLFTagResponse pHttpStatus_ =
  DeleteLFTagResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteLFTagResponse_httpStatus :: Lens.Lens' DeleteLFTagResponse Prelude.Int
deleteLFTagResponse_httpStatus = Lens.lens (\DeleteLFTagResponse' {httpStatus} -> httpStatus) (\s@DeleteLFTagResponse' {} a -> s {httpStatus = a} :: DeleteLFTagResponse)

instance Prelude.NFData DeleteLFTagResponse where
  rnf DeleteLFTagResponse' {..} = Prelude.rnf httpStatus
